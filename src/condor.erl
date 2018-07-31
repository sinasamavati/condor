-module(condor).

%% -----------------------------------------------------------------------------
%% API
%% -----------------------------------------------------------------------------
-export([start_listener/4]).
-export([stop_listener/1]).

start_listener(Name, Opts0, Mod, ModState) ->
    SupName = listener_sup_name(Name),
    Opts = maps:merge(default_opts(), Opts0),
    Res = condor_sup:start_listener_sup(SupName, Opts, Mod, ModState),
    start_acceptors(Res, SupName, Opts).

stop_listener(Name) ->
    SupName = listener_sup_name(Name),
    condor_sup:stop_listener_sup(SupName).

%% -----------------------------------------------------------------------------
%% internal
%% -----------------------------------------------------------------------------
default_opts() ->
    #{
       ip => {127, 0, 0, 1},
       max_acceptors => 100,
       len => 1
     }.

start_acceptors({ok, _}=Res, SupName, #{max_acceptors:=Max}) ->
    [condor_listener_sup:start_child(SupName) || _ <- lists:seq(1, Max)],
    Res;
start_acceptors(Else, _, _) ->
    Else.

listener_sup_name(Name) ->
    list_to_atom("condor_listener_sup_" ++ atom_to_list(Name)).
