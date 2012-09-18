-module(racon_cli).

-behaviour(gen_server).

-export([start_link/0]).
-export([get_gamelist/1, create_game/1, register_user/1,
         get_gamestate/4, make_move/4]).
-export([handle_call/3, handle_cast/2, handle_info/2,
         init/1,terminate/2]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_gamelist(HttpReq) ->
    gen_server:cast(?MODULE, {get_gamelist, HttpReq}).

create_game(HttpReq) ->
    gen_server:cast(?MODULE, {create_game, HttpReq}).

register_user(HttpReq) ->
    gen_server:cast(?MODULE, {register_user, HttpReq}).

get_gamestate(GID, none, State, HttpReq) ->
    gen_server:cast(?MODULE, {get_gamestate, GID, State, HttpReq});

get_gamestate(GID, User, State, HttpReq) ->
    gen_server:cast(?MODULE, {get_gamestate, GID, User, State, HttpReq}).

make_move(GID, User, Direction, HttpReq) ->
    gen_server:cast(?MODULE, {make_move, GID, User, Direction, HttpReq}).


init(_Args) ->
    {ok, {}}.

handle_cast({get_gamelist, HttpReq}, State) ->
    http_json_reply([ Id || {Id, _Nodes} <- get_games_mapping() ], HttpReq),
    {noreply, State};
handle_cast({create_game, HttpReq}, State) ->
    http_json_reply(create_new_game(), HttpReq),
    {noreply, State}.

handle_call(_Something, _From, State) ->
    {ok, State}.

handle_info(_Something, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%% Internal

get_games_mapping() ->
    [{1, none}, {2, none}].

create_new_game() ->
    ok.

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





