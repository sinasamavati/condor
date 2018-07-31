-module(condor).

%% -----------------------------------------------------------------------------
%% API
%% -----------------------------------------------------------------------------
-export([start_listener/4]).
-export([stop_listener/1]).

-type opts() :: #{
            ip => inet:ip_address(),
            port => inet:port_number(),
            len => 1 | 2 | 4,
            max_acceptors => non_neg_integer()
           }.
-export_type([opts/0]).

-spec start_listener(atom(), opts(), module(), term()) ->
                            {ok, pid()} | {error, term()}.
start_listener(Name, Opts0, Mod, InitialModState) ->
    SupName = listener_sup_name(Name),
    Opts = maps:merge(default_opts(), Opts0),
    Res = condor_sup:start_listener_sup(SupName, Opts, Mod, InitialModState),
    start_acceptors(Res, SupName, Opts).

-spec stop_listener(atom()) -> ok.
stop_listener(Name) ->
    SupName = listener_sup_name(Name),
    condor_sup:stop_listener_sup(SupName).

%% -----------------------------------------------------------------------------
%% internal
%% -----------------------------------------------------------------------------
default_opts() ->
    #{
       ip => {127, 0, 0, 1},
       len => 2,
       max_acceptors => 100
     }.

start_acceptors({ok, _}=Res, SupName, #{max_acceptors:=Max}) ->
    [condor_listener_sup:start_child(SupName) || _ <- lists:seq(1, Max)],
    Res;
start_acceptors(Else, _, _) ->
    Else.

listener_sup_name(Name) ->
    list_to_atom("condor_listener_sup_" ++ atom_to_list(Name)).
