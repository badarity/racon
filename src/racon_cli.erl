-module(racon_cli).

-behaviour(gen_server).

-export([start_link/0]).
-export([get_gamelist/0, gamepids/1, create_game/0, start_slave_game/4]).
-export([handle_call/3, handle_info/2, init/1, terminate/2]).
-export([prepare_mnesia/0]).

-include_lib("stdlib/include/ms_transform.hrl").

-record(state, {games = [], nodes = [node()]}). %% games = [{Pid, GID}]
-define(GAMES_TABLE, game_nodes).
-define(MNESIA_TIMEOUT, 120 * 1000).
-record(game_nodes, {gid :: integer,
                     master :: node() | down,
                     slave :: node() | down}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_gamelist() ->
    gen_server:call(?MODULE, get_gamelist).

create_game() ->
    gen_server:call(?MODULE, create_game).

start_master_game(MasterNode, GID, SlaveNode) ->
    gen_server:call({?MODULE, MasterNode}, {start_master_game, GID, SlaveNode}).

start_slave_game(SlaveNode, GID, MasterPid, Nonce) ->
    gen_server:call({?MODULE, SlaveNode}, {start_slave_game, GID, MasterPid, Nonce}).

gamepids(Gid) ->
    %% is it dirty?
    #game_nodes{master = Master, slave = Slave} = mt_get_game(Gid), %% FIXME
    {Res, _Bad} = gen_server:multi_call([Master, Slave], ?MODULE, {gamepid, Gid}),
    Result = fun(Node) -> proplists:get_value(Node, Res) end,
    {Result(Master), Result(Slave)}.

prepare_mnesia() ->
    application:start(mnesia),
    Fields = record_info(fields, game_nodes),
    {atomic, ok} = mnesia:create_table(?GAMES_TABLE, [{type, set},
                                                      {attributes, Fields}]).

init(_Args) ->
    fail_node_games(node()),
    greet_nodes(),
    process_flag(trap_exit, true),
    ok = mnesia:wait_for_tables([?GAMES_TABLE], ?MNESIA_TIMEOUT),
    {ok, #state{}}.

handle_call(get_gamelist, _From, State) ->
    {reply, ok_reply({ "games", mnesia:dirty_all_keys(?GAMES_TABLE) }), State};

handle_call(create_game, _From, State) ->
    {Reply, NewState} = create_new_game(State),
    {reply, Reply, NewState};

handle_call({start_master_game, GID, SlaveNode}, _From, State) ->
    {NewState, Pid} = start_master_game_process(GID, SlaveNode, State),
    {reply, Pid, NewState};

handle_call({start_slave_game, GID, MasterPid, Nonce}, _From, State) ->
    Pid = racon_game:start_link({slave, MasterPid, Nonce}),
    {reply, Pid, add_game(GID, Pid, slave, State)};

handle_call({gamepid, Gid}, _From, State) ->
    {reply, game_pid(Gid, State), State}.

handle_info({'EXIT', Pid, _Reason}, State) ->
    {ok, del_game(Pid, State)}.

terminate(_Reason, _State) ->
    ok.

%% Internal

greet_nodes() ->
    ok.


create_new_game(#state{nodes = Nodes} = State) ->
    {Master, Slave, NewNodes} = pick_game_nodes(Nodes),
    GID = store_game(Master, Slave),
    {Status, NewState} = 
	try_start_master_game(Master, GID, Slave, State#state{ nodes = NewNodes }),
    {{Status, { "gid", GID } }, NewState}.

try_start_master_game(Master, GID, Slave, State) ->
    try start_master_game_somewhere(Master, GID, Slave, State) of
        NewState -> {ok, NewState}
    catch
        _Error:Reason ->
            remove_game(GID),
            {error, State, Reason}
    end.

%% http client interaction

ok_reply(Reply) ->
    {ok, Reply}.

%% possibly rewrite
start_master_game_somewhere(Master, GID, Slave, State) when Master == node() ->
    start_master_game_process(GID, Slave, State);

start_master_game_somewhere(Master, GID, Slave, State) ->
    { start_master_game(Master, GID, Slave), State }.
    
start_master_game_process(GID, SlaveNode, State) ->
    Pid = racon_game:start_link({master, SlaveNode, GID}),
    add_game(GID, Pid, master, State).


pick_game_nodes([ Master ] = Nodes) ->
    {Master, undefined, Nodes };
pick_game_nodes([ Master, Slave | Tail]) ->
    {Master, Slave, Tail ++ [ Slave, Master ]}.

game_pid(GID, #state{games = Games}) ->
    {Pid, GID, _Type} = lists:keyfind(GID, 2, Games),
    Pid.

add_game(GID, Pid, Role, #state{games = Games} = State) ->
    State#state{ games = [ {Pid, GID, Role} | Games ]}.

del_game(Pid, #state{games = Games} = State) ->
    { value, {Pid, GID, Type}, NewGames } = lists:keytake(Pid, 1, Games),
    fail_game(GID, Type),
    State#state{games = NewGames}.

%% DB functions

mt_get_game(Gid) ->
    {atomic, Result} = mnesia:transaction(fun get_game/1, [Gid]),
    Result.

fail_node_games(Node) ->
    {atomic, _Res1} = mnesia:transaction(fun fail_games_as_master/1, [Node]),
    {atomic, _Res2} = mnesia:transaction(fun fail_games_as_slave/1, [Node]).

fail_games_as_master(Node) ->
    Games = ets:fun2ms(fun(#game_nodes{master = M} = Nodes) when M == Node -> Nodes end),
    fail_games(master, Games).

fail_games_as_slave(Node) ->
    Games = ets:fun2ms(fun(#game_nodes{slave = S} = Nodes) when S == Node -> Nodes end),
    fail_games(slave, Games).

fail_games(Role, GamesMatcher) ->
    [ fail_node(Game, Role) || Game <- mnesia:select(?GAMES_TABLE, GamesMatcher) ].


store_game(Master, Slave) ->
    {atomic, Gid} = mnesia:transaction(fun new_game/2, [Master, Slave]),
    Gid.

remove_game(Gid) ->
    mnesia:transaction(fun() -> mnesia:delete({?GAMES_TABLE, Gid}) end).

fail_game(GID, Type) ->
    Failer = fun() -> fail_node(get_game(GID), Type) end,
    mnesia:transaction(Failer).

get_game(Gid) ->
    [ Game | _ ] = mnesia:read(?GAMES_TABLE, Gid),
    Game.

fail_node(#game_nodes{gid = Gid, slave = undefined}, master) ->
    remove_game(Gid);
fail_node(#game_nodes{gid = Gid, master = undefined}, slave) ->
    remove_game(Gid);
fail_node(Nodes, master) ->
    mnesia:write(Nodes#game_nodes{master = undefined});
fail_node(Nodes, slave) ->
    mnesia:write(Nodes#game_nodes{slave = undefined}).


new_game(Master, Slave) ->
    Gid = gen_gid(),
    mnesia:write(#game_nodes{ gid = Gid,
                              master = Master,
                              slave = Slave }),
    Gid.

gen_gid() ->
    lists:max([ 0 | get_gids() ]) + 1.

get_gids() ->
    mnesia:all_keys(?GAMES_TABLE).
