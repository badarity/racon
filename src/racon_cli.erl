-module(racon_cli).

-behaviour(gen_server).

-export([start_link/0]).
-export([get_gamelist/1, create_game/1, register_user/2,
         get_gamestate/4, make_move/4]).
-export([handle_call/3, handle_cast/2, handle_info/2,
         init/1,terminate/2]).

-record(state, {games = [], nodes = [self()]}). %% games = [{Pid, GID}]
-define(GAMES_TABLE, game_nodes).
-define(MNESIA_TIMEOUT, 120 * 1000).
-record(game_nodes, {gid :: integer, nodes :: {node() | down, node() | down}}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_gamelist(HttpReq) ->
    gen_server:cast(?MODULE, {get_gamelist, HttpReq}).

create_game(HttpReq) ->
    gen_server:cast(?MODULE, {create_game, HttpReq}).

start_master_game(MasterNode, GID, SlaveNode) ->
    gen_server:call({?MODULE, MasterNode}, {start_master_game, GID, SlaveNode}).

start_slave_game(SlaveNode, GID, MasterPid) ->
    gen_server:call({?MODULE, SlaveNode}, {start_slave_game, GID, MasterPid}).

register_user(GID, HttpReq) ->
    gen_server:cast(?MODULE, {register_user, GID, HttpReq}).

get_gamestate(GID, none, State, HttpReq) ->
    gen_server:cast(?MODULE, {get_gamestate, GID, none, State, HttpReq});

get_gamestate(GID, User, State, HttpReq) ->
    gen_server:cast(?MODULE, {get_gamestate, GID, User, State, HttpReq}).

make_move(GID, User, Direction, HttpReq) ->
    gen_server:cast(?MODULE, {make_move, GID, User, Direction, HttpReq}).

%% specifying GID multiple times is ugly ;(
game_register_user(GID) ->
    game_call(GID, {game_register_user, GID}).

game_get_state(GID, User, PrevState) ->
    game_cast(GID, {game_get_state, GID, User, PrevState}).

game_make_move(GID, User, Direction) ->
    game_cast(GID, {game_make_move, GID, User, Direction}).


init(_Args) ->
    fail_node_games(node()),
    greet_nodes(),
    ok = mnesia:wait_for_tables([?GAMES_TABLE], ?MNESIA_TIMEOUT),
    {ok, #state{}}.

handle_cast({get_gamelist, HttpReq}, State) ->
    http_json_reply([ Id || {Id, _Nodes} <- get_games_mapping() ], HttpReq),
    {noreply, State};

handle_cast({create_game, HttpReq}, State) ->
    {Reply, NewState} = create_new_game(State),
    http_reply(Reply, HttpReq),
    {noreply, NewState};

handle_cast({register_user, GID, HttpReq}, State) ->
    http_reply(game_register_user(GID), HttpReq),
    {noreply, State};

handle_cast({get_gamestate, GID, User, PrevState, HttpReq}, State) ->
    {noreply, http_lpoll_reply(game_get_state(GID, User, PrevState), HttpReq, State)};

handle_cast({make_move, GID, User, Direction, HttpReq}, State) ->
    http_reply(game_make_move(GID, User, Direction), HttpReq),
    {noreply, State};

handle_cast({game_get_state, GID, User, PrevState}, State) ->
    racon_game:get_state(game_pid(GID, State), User, PrevState),
    {noreply, State};

handle_cast({game_make_move, GID, User, Direction}, State) ->
    racon_game:get_state(game_pid(GID, State), User, Direction),
    {noreply, State}.

handle_call({start_master_game, GID, SlaveNode}, _From, State) ->
    Pid = racon_game:start_link({master, SlaveNode}),
    {reply, Pid, add_game(GID, Pid, State)};

handle_call({start_slave_game, GID, MasterPid}, _From, State) ->
    Pid = racon_game:start_link({slave, MasterPid}),
    {reply, Pid, add_game(GID, Pid, State)};

handle_call({game_register_user, GID}, From, State) ->
    racon_game:register_user(game_pid(GID, State), From),
    {noreply, State}.


handle_info({'EXIT', Pid, _Reason}, State) ->
    {ok, del_game(Pid, State)}.

terminate(_Reason, _State) ->
    ok.

%% Internal

get_games_mapping() ->
    [{1, none}, {2, none}].

create_new_game(#state{nodes = Nodes} = State) ->
    {Master, Slave, NewNodes} = pick_game_nodes(Nodes),
    GID = store_game(Master, Slave),
    { try_start_master_game(Master, GID, Slave),
      State#nodes{ nodes = NewNodes } }.

try_start_master_game(Master, GID, Slave) ->
    try start_master_game(Master, GID, Slave) of
        Pid -> {ok, Pid}
    catch
        _Error:Reason ->
            remove_game(GID),
            {error, Reason}
    end.

pick_game_nodes([ Master | Others ]) ->
    [ Slave | Tail ] = Others ++ [Master],
    {Master, Slave, Tail ++ [Slave] }.

game_pid(GID, #state{games = Games}) ->
    {Pid, GID, _Type} = lists:keyfind(GID, 2, Games),
    Pid.

add_game(GID, Pid, Role, #state{games = Games} = State) ->
    State#state{ games = [ {Pid, GID, Role} | Games ]}.

del_game(Pid, #state{games = Games} = State) ->
    { value, {Pid, GID, Type}, NewGames } = lists:keytake(Pid, 1, Games),
    fail_game(GID, Type),
    State#state{games = NewGames}.

fail_game(GID, Type) ->
    ok. %% remove from mnesia

game_cast(GID, Msg) ->
    case get_game_node(GID) of
        {ok, Node} ->
            gen_server:cast({Node, ?MODULE}, Msg),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

game_call(GID, Msg) ->
    TryCall = fun({_Node, _T}, ok) -> ok;
                 ({Node, _T}, _Error) -> failsafe_call({Node, ?MODULE}, Msg)
              end,
    NoGamesErr = {error, "No nodes are running this game"},
    lists:foldl(TryCall, NoGamesErr, get_games(GID)).

failsafe_call(Where, Msg) ->
    try gen_server:call(Where, Msg) of
        _Success -> ok
    catch
        _Error:Reason -> {error, Reason}
    end.


http_reply({ok, Reply}, HttpReq) ->
    http_json_reply(Reply, HttpReq);
http_reply({error, Reason}, HttpReq) ->
    http_error_reply(Reason, HttpReq).

http_error_reply(Reason, HttpReq) ->
    HttpReq:respond({501, [], ">[ IMPOSIBRU"}).

%% DB functions

fail_node_games(Node) ->
    NodeGames = [{#game_nodes{nodes={$1, $2}},
                  [{'==', $1, Node}, {'==', $2, Node}]}]
    GamesFailer =
        fun() ->
                mnesia:select(?GAMES_TABLE, NodeGames)

%% JSON related

http_json_reply(Reply, HttpReq) ->
    HttpReq:respond({200, [], json_encode(Reply)}).

json_encode(Data) ->
    mochijson2:encode(term_to_mochijson(Data)).

term_to_mochijson({Key, Value}) ->
    {struct, [ {Key, term_to_mochijson(Value)} ]};
term_to_mochijson(ValueList) when is_list(ValueList) ->
    lists:map(fun term_to_mochijson/1, ValueList);
term_to_mochijson(Value) ->
    Value.

