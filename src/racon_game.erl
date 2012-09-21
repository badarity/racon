-module(racon_game).

-behaviour(gen_server).

-export([start_monitor/1, gamestate/2, move/2]).
-export([init/1, handle_call/3]).

-record(state, {type :: master | slave,
                field_size :: {integer(), integer()},
                master_pid,
                slave_pid,
                id_map,
                positions}).

start_monitor(Args) ->
    {ok, Pid} = gen_server:start(?MODULE, Args, []),
    erlang:monitor(process, Pid),
    Pid.

gamestate(Pid, Uid) ->
    gen_server:call(Pid, {gamestate, Uid}).

move(Pid, Direction) ->
    gen_server:call(Pid, {move, Direction}).


init({master, SlaveNode, Gid}) ->
    Slave = start_slave(SlaveNode, Gid),
    InitState = init_state(master),
    {ok, InitState#state{slave_pid = Slave, master_pid = self()}};

init({slave, Master}) ->
    InitState = init_state(slave),
    {ok, InitState#state{slave_pid = self(), master_pid = Master}}.

handle_call({gamestate, Uid}, From, #state{type = master, slave_pid = Slave} = State) ->
    slave_gamestate(Slave, Uid),
    handle_gamestate(Uid, From, State);

handle_call({gamestate, Uid}, From, #state{type = slave} = State) ->
    handle_gamestate(Uid, From, State);

handle_call({move, Direction}, From, #state{type = master, slave_pid = Slave} = State) ->
    slave_move(Slave, Direction),
    handle_move(Direction, From, State);

handle_call({move, Direction}, From, #state{type = slave} = State) ->
    handle_move(Direction, From, State).

%% internal

init_state(Type) ->
    #state{type = Type}.

start_slave(undefined, _Gid) ->
    undefined;
start_slave(Node, Gid) ->
    Starter = spawn_link(Node, fun() -> slave_starter(self(), Gid) end),
    receive
        {Starter, Slave} ->
            Slave
    end.

slave_starter(Client, Gid) ->
    {ok, Pid} = racon_game:start_monitor({slave, Client}),
    Client ! {self(), Pid}.

slave_gamestate(undefined, _Uid) ->
    ok;
slave_gamestate(Slave, Uid) ->
    try gamestate(Slave, Uid) of
        Success -> Success
    catch
        _Error:Reason ->
            {error, Reason}
    end.

slave_move(undefined, _Direction) ->
    ok;
slave_move(Slave, Direction) ->
    try move(Slave, Direction) of
        Success -> Success
    catch
        _Error:Reason ->
            {error, Reason}
    end.

handle_gamestate(undefined, {_Ref, Client} = From, State) ->
    {reply, ok, State}.

handle_move(undefined, {_Ref, Client} = From, State) ->
    {reply, ok, State}.
