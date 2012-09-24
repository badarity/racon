-module(racon_game).

-behaviour(gen_server).

-export([start_link/1, gamestate/2, move/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([uid_encode/1, uid_decode/1]).
-export([uid_coding_test/0]).

-define(NONCE_SIZE, 16).

-record(state, {type :: master | slave,
                field_size :: {integer(), integer()},
		nonce,
                master_pid,
                slave_pid,
                users = [],
                positions = []}).

start_link(Args) ->
    {ok, Pid} = gen_server:start_link(?MODULE, Args, []),
    Pid.

gamestate(Pid, Uid) ->
    gamestate(Pid, self(), Uid).

gamestate(Pid, Caller, Uid) ->
    gen_server:cast(Pid, {gamestate, Caller, Uid}).

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

handle_cast({gamestate, Caller, Uid}, #state{type = master, slave_pid = Slave} = State) ->
    slave_gamestate(Slave, Caller, Uid),
    handle_gamestate(Uid, Caller, State);

handle_cast({gamestate, Caller, Uid}, #state{type = slave} = State) ->
    handle_gamestate(Uid, Caller, State).

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

slave_gamestate(undefined, _Caller, _Uid) ->
    ok;
slave_gamestate(Slave, Caller, Uid) ->
    try gamestate(Slave, Caller, Uid) of
        Success -> Success
    catch
        _Error:Reason ->
            {error, Reason}
    end.

slave_move(undefined, _Uid, _Direction) ->
    ok;
slave_move(Slave, Uid, Direction) ->
    try move(Slave, Uid, Direction) of
        Success -> Success
    catch
        _Error:Reason ->
            {error, Reason}
    end.

handle_gamestate(Uid, Client, State) ->
    NewState = add_client(Uid, Client, State),
    notify_clients(NewState),
    {noreply, NewState}.

handle_move(Uid, Direction, {Client, _Ref}, State) ->
    NewState = move_player(Uid, Direction, State),
    notify_clients(NewState),
    {reply, empty, add_client(Uid, Client, NewState)}.

move_player(Uid, Direction, #state{positions = Pos} = State) ->
    PlayerPos = proplists:get_value(Uid, Pos),
    change_pos(Uid, PlayerPos, Direction, State).

change_pos(_Uid, undefined, _Direction, State) ->
    State;
change_pos(_Uid, {_X, 1}, up, #state{field_size = {_W, _H}} = State) ->
    State;
change_pos(_Uid, {1, _Y}, left, #state{field_size = {_W, _H}} = State) ->
    State;
change_pos(_Uid, {_X, Y}, down, #state{field_size = {_W, Y}} = State) ->
    State;
change_pos(_Uid, {X, _Y}, right, #state{field_size = {X, _H}} = State) ->
    State;
change_pos(Uid, Coords, Direction, #state{positions = Pos} = State) ->
    NewCoords = step(Coords, Direction),
    CleanedPos = lists:keydelete(NewCoords, 2, Pos),
    State#state{positions = [ proplists:append_values(Uid, NewCoords) | 
                              proplists:delete(Uid, CleanedPos) ]}.

step({X, Y}, up) ->
    {X, Y - 1};
step({X, Y}, down) ->
    {X, Y + 1};
step({X, Y}, left) ->
    {X - 1, Y};
step({X, Y}, right) ->
    {X + 1, Y}.

handle_down(Pid, #state{master_pid = Pid} = State) ->
    State#state{master_pid = undefined};

handle_down(Pid, #state{slave_pid = Pid} = State) ->
    State#state{slave_pid = undefined}.

compose_gamestate(undefined, #state{field_size = Size,
                                         positions = Pos} = State) ->
    {Uid, NewState} = gen_uid(State),
    NewPos = place_new_player(Uid, Size, Pos),
    { add_property({"uid", Uid}, compose_field(Size, Uid, NewPos)),
      NewState#state{positions = NewPos} };

compose_gamestate(Uid, #state{field_size = Size, positions = Pos} = State) ->
    { compose_field(Size, Uid, Pos), State }.

compose_field({H, W}, Uid, Pos) ->
    {object, [ { "field", 
                 {object, [ { "height", H }, { "width", W } ] } },
               { "players", compose_positions(Uid, Pos) } ]}.

compose_positions(CurrentUid, Positions) ->
    Folder =
        fun({Uid, Position}, {PlayerId, Mapped}) when CurrentUid == Uid ->
                {PlayerId, compose_position(PlayerId, Position, Mapped)};
            ({_Uid, Position}, {PlayerId, Mapped}) ->
                {PlayerId + 1, compose_position(PlayerId, Position, Mapped)}
        end,
    {_LastId, MappedPositions} = lists:foldl(Folder, {1, []}, Positions),
    MappedPositions.

compose_position(PlayerId, {X, Y}, Composed) ->
    [ {object, [ {"id", PlayerId},
                 {"pos", {object, [{"x", X}, {"y", Y}] } } ] }
      | Composed ].

add_property(Property, {object, Props}) ->
    {object, [Property | Props]}.

place_new_player(Uid, Size, Pos) ->
    place_player(Uid, pick_free_position(Size, Pos), Pos).

place_player(_Uid, undefined, Occupied) ->
    Occupied;

place_player(Uid, Position, Occupied) ->
    [ {Uid, Position} | Occupied ].

pick_free_position({H, W}, Occupied) ->
    Seq = fun lists:seq/2,
    {_, Free} = lists:splitwith(fun(Pos) -> lists:keymember(Pos, 2, Occupied) end,
                                [{X, Y} || X <- Seq(1, H), Y <- Seq(1, W)]),
    case Free of
        [] -> undefined;
        [ Affordable | _T ] -> Affordable
    end.              

add_client(Uid, Client, #state{users = Users} = State) ->
    State#state{users = lists:keystore(Client, 1, Users, {Client, Uid})}.

notify_clients(#state{users = Users} = State) ->
    lists:foldl(fun notifier/2, State, Users).

notifier({Pid, Uid}, CurrentState) ->
    {NextGamestate, NextState} = compose_gamestate(Uid, CurrentState),
    Pid ! {gamestate, NextGamestate},
    NextState.

gen_uid(#state{nonce = Nonce} = State) ->
    NewNonce = Nonce + 1,
    {int_to_uuid(Nonce), State#state{nonce = NewNonce}}.

int_to_uuid(Integer) ->
    uid_encode(crypto:sha(term_to_binary(Integer))).

uid_encode(<<Hash:160, _Rest/binary>>) ->
    list_to_binary(integer_to_list(Hash, 16)).

uid_decode(UidBinaryString) ->
    Uid = list_to_integer(binary_to_list(UidBinaryString), 16),
    <<Uid:160>>.

gen_nonce() ->
    <<Int:?NONCE_SIZE/unit:8,_Rest/binary>> = crypto:rand_bytes(?NONCE_SIZE),
    Int.

uid_coding_test() ->
    Raw = crypto:sha(<<"Arbeit macht frei">>),
    Raw = uid_decode(uid_encode(Raw)).
