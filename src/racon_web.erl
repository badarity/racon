%% @doc Web server for racon.

-module(racon_web).
-author("Nikita Lebedev <nlebedev@reksoft.ru>").

-export([start/1, stop/0]).

%% External API

start(Options) ->
    DocRoot = proplists:get_value(docroot, Options),
    Port = proplists:get_value(port, Options),
    Mimetypes = [{<<".html">>, [<<"text/html">>]}],
    Dispatch =
	[
	 {'_', [
		{[<<"game">>, gid], racon_game_handler, []},
		{[<<"game">>], racon_http_handler, [gamelist]},
		{[<<"newgame">>], racon_http_handler, [newgame]},
		{['...'], cowboy_static, [{directory, DocRoot},
					  {file, <<"index.html">>},
					   {mimetypes, Mimetypes}]}
	       ]}
	],
    {ok, _} = cowboy:start_http(?MODULE, 100, [{port, Port}], [
							       {dispatch, Dispatch}
							      ]).


stop() ->
    cowboy:stop_listener(?MODULE).

%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.


%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

you_should_write_a_test() ->
    ?assertEqual(
       "No, but I will!",
       "Have you written any tests?"),
    ok.

-endif.
