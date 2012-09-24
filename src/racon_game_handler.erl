-module(racon_game_handler).

-export([init/3]).
-export([websocket_init/3, websocket_handle/3,
        websocket_info/3, websocket_terminate/3]).

-record(state, {master, slave, game, uid = undefined}).

init({tcp,http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
    { Gid, Req2 } = get_gid(Req),
    { Uid, Req3 } = get_uid(Req2),
    State = connect_games(Gid),
    report_gamestate(Uid, State),
    {ok, Req3, State}.

websocket_handle({text, Msg}, Req, #state{game = Pid} = State) ->
    make_move(Msg, Pid),
    {ok, Req, State}.

websocket_info({gamestate, Gamestate}, Req, State) ->
    {reply, {text, json_encode(Gamestate)}, Req, State};

websocket_info({'DOWN', _Ref, _Type, Pid, _Info}, Req, State) ->
    say_bye(Pid, Req, State).

websocket_terminate(_Reason, _Req, _S) ->
    ok.

get_gid(Req) ->
    {Gid, Req2} = cowboy_req:binding(gid, Req),
    { list_to_integer(binary_to_list(Gid)), Req2 }.

get_uid(Req) ->
    { Uid, Req2 } = cowboy_req:qs_val(<<"uid">>, Req),
    { maybe_list(Uid), Req2 }.

maybe_list(undefined) ->
    undefined;
maybe_list(Binary) ->
    binary_to_list(Binary).

connect_games(Gid) ->
    {Master, Slave} = racon_cli:gamepids(Gid),
    lists:foreach(fun(Node) -> erlang:monitor(process, Node) end,
                  [Master, Slave]),
    #state{master = Master, slave = Slave, game = Master}.

report_gamestate(Uid, #state{game = Pid} = State) ->
    racon_game:gamestate(Pid, Uid).%% it will send back async response

make_move(Direction, GamePid) ->
    racon_game:move(GamePid, direction(Direction)).

direction(<<"up">>) -> up;
direction(<<"down">>) -> down;
direction(<<"left">>) -> left;
direction(<<"right">>) -> right.

say_bye(Pid, Req, #state{master = Pid, slave = undefined} = State) ->
    no_nodes(Req, State);
say_bye(Pid, Req, #state{slave = Pid, game = Pid} = State) ->
    no_nodes(Req, State);
say_bye(Pid, Req, #state{master = Pid, slave = SlavePid} = State) ->
    {ok, Req, State#state{game = SlavePid, master = undefined}};
say_bye(Pid, Req, #state{slave = Pid} = State) ->
    {ok, Req, State#state{slave = undefined}}.

no_nodes(Req, State) ->
    {shutdown, Req, State#state{game = undefined}}.


%% Json

json_encode(Data) ->
    list_to_binary(mochijson2:encode(term_to_mochijson(Data))).

term_to_mochijson({Key, Value}) ->
    {struct, [ {Key, term_to_mochijson(Value)} ]};
term_to_mochijson(ValueList) when is_list(ValueList) ->
    lists:map(fun term_to_mochijson/1, ValueList);
term_to_mochijson(Value) ->
    Value.

