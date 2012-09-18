%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc racon-backend.

-module(racon).
-author("Mochi Media <dev@mochimedia.com>").
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.


%% @spec start() -> ok
%% @doc Start the racon-backend server.
start() ->
    racon_deps:ensure(),
    ensure_started(crypto),
    application:start(racon).


%% @spec stop() -> ok
%% @doc Stop the racon-backend server.
stop() ->
    application:stop(racon).
