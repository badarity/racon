-module(racon_http_handler).

%%-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/2]).

init({tcp, http}, Req, [Operation]) ->
    {ok, Req, Operation}.

handle(Req, Operation) ->
    {ok, dispatch_req(Operation, Req), Operation}.

terminate(_Req, _State) ->
	ok.

%% Internal

dispatch_req(newgame, Req) ->
    reply(racon_cli:create_game(), Req);
dispatch_req(gamelist, Req) ->
    reply(racon_cli:get_gamelist(), Req).

reply(Value, Req) ->
    {ok, Req2} = http_reply({ok, Value}, Req),
    Req2.


http_reply({ok, Reply}, HttpReq) ->
    http_json_reply(Reply, HttpReq);
http_reply({error, Reason}, HttpReq) ->
    http_error_reply(Reason, HttpReq).

http_json_reply(Reply, HttpReq) ->
    cowboy_req:reply(200, [], json_encode(Reply), HttpReq).

http_error_reply(Reason, HttpReq) ->
    cowboy_req:reply(501, [], <<">[ IMPOSIBRU">>, HttpReq).


%% JSON related

json_encode(Data) ->
    mochijson2:encode(term_to_mochijson(Data)).

term_to_mochijson({Key, Value}) ->
    {struct, [ {Key, term_to_mochijson(Value)} ]};
term_to_mochijson(ValueList) when is_list(ValueList) ->
    lists:map(fun term_to_mochijson/1, ValueList);
term_to_mochijson(Value) ->
    Value.

-define(MAP(Dir), {??Dir, Dir}).

