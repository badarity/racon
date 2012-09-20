-module(racon_cli).

-behaviour(gen_server).

-export([start_link/0]).
-export([get_gamelist/0, create_game/0, start_slave_game/3]).
-export([handle_call/3, handle_info/2, init/1, terminate/2]).
-export([prepare_mnesia/0]).

-include_lib("stdlib/include/ms_transform.hrl").

-record(state, {games = [], nodes = [self()]}). %% games = [{Pid, GID}]
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

start_slave_game(SlaveNode, GID, MasterPid) ->
    gen_server:call({?MODULE, SlaveNode}, {start_slave_game, GID, MasterPid}).

prepare_mnesia() ->
    application:start(mnesia),
    Fields = record_info(fields, game_nodes),
    {atomic, ok} = mnesia:create_table(?GAMES_TABLE, [{type, set},
                                                      {attributes, Fields}]).

init(_Args) ->
    prepare_mnesia(),
    fail_node_games(node()),
    greet_nodes(),
    ok = mnesia:wait_for_tables([?GAMES_TABLE], ?MNESIA_TIMEOUT),
    {ok, #state{}}.

handle_call(get_gamelist, _From, State) ->
    {reply, [ Id || {Id, _Nodes} <- get_games_mapping() ], State};

handle_call(create_game, _From, State) ->
    {Reply, NewState} = create_new_game(State),
    {reply, Reply, NewState};

handle_call({start_master_game, GID, SlaveNode}, _From, State) ->
    Pid = racon_game:start_link({master, SlaveNode}),
    {reply, Pid, add_game(GID, Pid, master, State)};

handle_call({start_slave_game, GID, MasterPid}, _From, State) ->
    Pid = racon_game:start_link({slave, MasterPid}),
    {reply, Pid, add_game(GID, Pid, slave, State)}.

handle_info({'EXIT', Pid, _Reason}, State) ->
    {ok, del_game(Pid, State)}.

terminate(_Reason, _State) ->
    ok.

%% Internal

greet_nodes() ->
    ok.

get_games_mapping() ->
    [{1, none}, {2, none}].

create_new_game(#state{nodes = Nodes} = State) ->
    {Master, Slave, NewNodes} = pick_game_nodes(Nodes),
    GID = store_game(Master, Slave),
    { try_start_master_game(Master, GID, Slave),
      State#state{ nodes = NewNodes } }.

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

%% DB functions

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
    mnesia:delete({?GAMES_TABLE, Gid}).

fail_game(GID, Type) ->
    Failer = fun() -> fail_node(get_game(GID), Type) end,
    mnesia:transaction(Failer).

get_game(Gid) ->
    mnesia:read(?GAMES_TABLE, Gid).

fail_node(#game_nodes{gid = Gid, slave = undefined}, master) ->
    remove_game(Gid);
fail_node(#game_nodes{gid = Gid, master = undefined}, slave) ->
    remove_game(Gid);
fail_node(Nodes, master) ->
    mnesia:write(Nodes#game_nodes{master = undefined});
fail_node(Nodes, slave) ->
    mnesia:write(Nodes#game_nodes{slave = undefined}).


new_game(Master, Slave) ->
    mnesia:write(#game_nodes{ gid = gen_gid(),
                              master = Master,
                              slave = Slave }).

gen_gid() ->
    lists:max([ 0 | mnesia:all_keys(?GAMES_TABLE) ]) + 1.
    
