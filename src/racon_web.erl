%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc Web server for racon.

-module(racon_web).
-author("Mochi Media <dev@mochimedia.com>").

-export([start/1, stop/0, loop/2]).

%% External API

start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req, DocRoot) ->
    "/" ++ Path = Req:get(path),
    try
        dispatch_request(Req)
    catch
        Type:What ->
            Report = ["web request failed",
                      {path, Path},
                      {type, Type}, {what, What},
                      {trace, erlang:get_stacktrace()}],
            error_logger:error_report(Report),
            %% NOTE: mustache templates need \ because they are not awesome.
            Req:respond({500, [{"Content-Type", "text/plain"}],
                         "request failed, sorry\n"})
    end.

%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

dispatch_request(Req) ->
    case { Req:get(path),Req:get(method) } of
        { "/games", 'GET' } ->
            racon_cli:get_gamelist(Req);
        { "/games", 'PUT' } ->
            racon_cli:create_game(Req);
        { "/reg/" ++ GID, 'GET' } ->
            racon_cli:register_user(GID, Req);
        { "/games/" ++ GID, 'GET' } ->
            get_gamestate(GID, Req);
        { "/games/" ++ GID, 'PUT' } ->
            make_move(GID, Req);
        _ ->
            Req:respond({501, [], []})
    end.

get_req_option(Key, Req) ->
    proplists:lookup(Key, Req:parse_qs()).

get_gamestate(GID, Req) ->
    racon_cli:get_gamestate(GID,
                            get_req_option("user", Req),
                            get_req_option("state", Req),
                            Req).

-define(MAP(Dir), {??Dir, Dir}).
make_move(GID, Req) ->
    Directions = [ "up", "down", "left", "right" ],
    DirMap =
        fun(Dir) -> true = lists:member(Dir, Directions), list_to_atom(Dir) end,
    User = get_req_option("user", Req),
    Direction = DirMap(get_req_option("direction", Req)),
    racon_cli:make_move(GID, User, Direction, Req).


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
