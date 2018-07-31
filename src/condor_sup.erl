-module(condor_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([start_listener_sup/4]).
-export([stop_listener_sup/1]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_listener_sup(SupName, Opts, Mod, ModState) ->
    supervisor:start_child(?MODULE, [SupName, Opts, Mod, ModState]).

stop_listener_sup(SupName) ->
    condor_listener_sup:kill_children(SupName),
    exit(whereis(SupName), kill),
    ok.

%% -----------------------------------------------------------------------------
%% supervisor callback
%% -----------------------------------------------------------------------------
init([]) ->
    {ok, {
       {simple_one_for_one, 60, 120},
       [{condor_listener_sup, {condor_listener_sup, start_link, []},
         temporary, 1000, supervisor, [condor_listener_sup]}]
      }}.
