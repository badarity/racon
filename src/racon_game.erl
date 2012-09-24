-module(racon_game).

-behaviour(gen_server).

-export([start_link/1, gamestate/2, move/3]).
-export([init/1, handle_call/3, handle_info/2, terminate/2]).

-define(NONCE_SIZE, 16).

-record(state, {type :: master | slave,
                field_size :: {integer(), integer()},
		nonce,
                master_pid,
                slave_pid,
                users = [],
                positions}).

start_link(Args) ->
    {ok, Pid} = gen_server:start_link(?MODULE, Args, []),
    Pid.

gamestate(Pid, Uid) ->
    gen_server:call(Pid, {gamestate, Uid}).

move(Pid, Uid, Direction) ->
    gen_server:call(Pid, {move, Uid, Direction}).


init({master, SlaveNode, Gid}) ->
    Nonce = gen_nonce(),
    Slave = start_slave(SlaveNode, Gid, Nonce),
    InitState = init_state(master),
    {ok, InitState#state{slave_pid = Slave, master_pid = self(), nonce = Nonce}};

init({slave, Master, Nonce}) ->
    InitState = init_state(slave),
    erlang:monitor(process, Master),
    {ok, InitState#state{slave_pid = self(), master_pid = Master, nonce = Nonce}}.

handle_call({gamestate, Uid}, From, #state{type = master, slave_pid = Slave} = State) ->
    slave_gamestate(Slave, Uid),
    handle_gamestate(Uid, From, State);

handle_call({gamestate, Uid}, From, #state{type = slave} = State) ->
    handle_gamestate(Uid, From, State);

handle_call({move, Uid, Direction}, From,
            #state{type = master, slave_pid = Slave} = State) ->
    slave_move(Slave, Uid, Direction),
    handle_move(Uid, Direction, From, State);

handle_call({move, Uid, Direction}, From, #state{type = slave} = State) ->
    handle_move(Uid, Direction, From, State).

handle_info({'DOWN', _Type, Pid, _Info}, State) ->
    handle_down(Pid, State).

terminate(_Reason, _State) ->
    ok.
    

%% internal

init_state(Type) ->
    #state{type = Type, field_size = {30, 30}}.

start_slave(undefined, _Gid, _Nonce) ->
    undefined;
start_slave(Node, Gid, Nonce) ->
    Pid = racon_cli:start_slave_game(Node, Gid, self(), Nonce),
    erlang:monitor(process, Pid),
    Pid.

slave_gamestate(undefined, _Uid) ->
    ok;
slave_gamestate(Slave, Uid) ->
    try gamestate(Slave, Uid) of
        Success -> Success
    catch
        _Error:Reason ->
            {error, Reason}
    end.

slave_move(undefined, Uid, _Direction) ->
    ok;
slave_move(Slave, Uid, Direction) ->
    try move(Slave, Uid, Direction) of
        Success -> Success
    catch
        _Error:Reason ->
            {error, Reason}
    end.

handle_gamestate(Uid, {Client, _Ref}, State) ->
    {reply, fake_gamestate(Uid, Client, State), add_client(Uid, Client, State)}.

handle_move(Uid, Direction, {Client, _Ref}, State) ->
    notify_clients(State),
    {reply, empty, add_client(Uid, Client, State)}.

handle_down(Pid, #state{master_pid = Pid} = State) ->
    State#state{master_pid = undefined};

handle_down(Pid, #state{slave_pid = Pid} = State) ->
    State#state{slave_pid = undefined}.

fake_gamestate(undefined, Pid, #state{field_size = {H, W}} = State) ->
    Uid = gen_uid(State),
    { Uid, [ fake_field(H, W) ]};

fake_gamestate(_Uid, Pid, #state{field_size = {H, W}}) ->
    fake_field(H, W).


fake_field(H, W) ->
    [ { "field", [ { "height", H }, { "width", W } ] },
      { "players", [ { 0, [0,0] }, { 1, [3, 3] } ] } ].

add_client(Uid, Client, #state{users = Users} = State) ->
    State#state{users = lists:keystore(Client, 1, Users, {Client, Uid})}.

notify_clients(#state{users = Users} = State) ->
    [ Pid ! fake_gamestate(Uid, Pid, State) || {Uid, Pid} <- Users].

gen_uid(#state{nonce = Nonce} = State) ->
    NewNonce = Nonce + 1,
    {int_to_uuid(Nonce), State#state{nonce = Nonce}}.

int_to_uuid(Integer) ->
    uri_encode(crypto:sha(term_to_binary(Integer))).

uri_encode(<<Hash:160, _Rest/binary>>) ->
    list_to_binary(lists:flatten(io_lib:format("~.16B"))).

gen_nonce() ->
    crypto:rand_bytes(?NONCE_SIZE).
