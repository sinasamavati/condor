-module(condor_listener_sup).
-behaviour(supervisor).

-export([start_link/4]).
-export([start_child/1]).
-export([kill_children/1]).

-export([init/1]).

start_link(SupName, Opts, Mod, ModState) ->
    #{port := Port} = Opts,
    supervisor:start_link(
      {local, SupName}, ?MODULE, [Port, Opts, Mod, ModState]
     ).

start_child(SupName) ->
    supervisor:start_child(SupName, []).

kill_children(SupName) ->
    [exit(Pid, kill) ||
        {_, Pid, _, _} <- supervisor:which_children(SupName)],
    ok.

%% -----------------------------------------------------------------------------
%% supervisor callback
%% -----------------------------------------------------------------------------
init([Port, Opts, Mod, ModState]) ->
    #{ip := IP} = Opts,
    {ok, LSock} = gen_tcp:listen(Port, [{ip, IP}, {active, once}, binary]),
    {ok, {
       {simple_one_for_one, 60, 120},
       [{condor_listener, {condor_listener, start_link, [LSock, Mod, ModState, Opts]},
         permanent, 1000, worker, [condor_listener]}]
      }}.
