%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc Supervisor for the racon application.

-module(racon_sup).
-author("Mochi Media <dev@mochimedia.com>").

-behaviour(supervisor).

%% External exports
-export([start_link/0, upgrade/0]).

%% supervisor callbacks
-export([init/1]).

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec upgrade() -> ok
%% @doc Add processes if necessary.
upgrade() ->
    {ok, {_, Specs}} = init([]),

    Old = sets:from_list(
            [Name || {Name, _, _, _} <- supervisor:which_children(?MODULE)]),
    New = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
    Kill = sets:subtract(Old, New),

    sets:fold(fun (Id, ok) ->
                      supervisor:terminate_child(?MODULE, Id),
                      supervisor:delete_child(?MODULE, Id),
                      ok
              end, ok, Kill),

    [supervisor:start_child(?MODULE, Spec) || Spec <- Specs],
    ok.

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->
    Client = client_specs(),
    Processes = [Client],
    Strategy = {one_for_all, 10, 10},
    racon_web:start(web_config(8081)),
    {ok,
     {Strategy, lists:flatten(Processes)}}.

web_config(Port) ->
    [{ip, {0,0,0,0}}, {port, Port}, {docroot, "priv/www"}].

web_specs(Mod, Port) ->
    {Mod,
     {Mod, start, [web_config(Port)]},
     temporary, 5000, worker, dynamic}.

client_specs() ->
    {racon_cli, {racon_cli, start_link, []},
     permanent, 5000, worker, dynamic}.
