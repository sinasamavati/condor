-module(condor).

%% -----------------------------------------------------------------------------
%% API
%% -----------------------------------------------------------------------------
-export([start_listener/4]).
-export([stop_listener/1]).

-type opts() :: #{
            ip => inet:ip_address(),
            port => inet:port_number(),
            len => 1 | 2,
            timeout => non_neg_integer(),
            max_acceptors => non_neg_integer()
           }.
-export_type([opts/0]).

-spec start_listener(atom(), opts(), module(), term()) ->
                            {ok, pid()} | {error, term()}.
start_listener(Name, Opts0, Mod, InitialModState) ->
    SupName = listener_sup_name(Name),
    Opts = maps:merge(default_opts(), Opts0),
    ok = check_opts(Opts),
    Res = condor_sup:start_listener_sup(SupName, Opts, Mod, InitialModState),
    start_acceptors(Res, SupName, Opts).

-spec stop_listener(atom()) -> ok.
stop_listener(Name) ->
    SupName = listener_sup_name(Name),
    condor_sup:stop_listener_sup(SupName).

%% -----------------------------------------------------------------------------
%% internal
%% -----------------------------------------------------------------------------
-spec default_opts() -> opts().
default_opts() ->
    #{
       ip => {127, 0, 0, 1},
       len => 2,
       timeout => 10000,
       max_acceptors => 100
     }.

-spec check_opts(opts()) -> ok | no_return().
check_opts(#{len:=Len}) when Len =< 0; Len > 2 ->
    exit(badlen);
check_opts(_) ->
    ok.

-spec start_acceptors(Res, atom(), opts()) ->
                             Res when Res :: {ok, pid()}
                                           | {error, term()}.
start_acceptors({ok, _}=Res, SupName, #{max_acceptors:=Max}) ->
    start_acceptor_n(SupName, Max),
    Res;
start_acceptors(Else, _, _) ->
    Else.

-spec start_acceptor_n(atom(), integer()) -> ok.
start_acceptor_n(_, 0) ->
    ok;
start_acceptor_n(SupName, N) ->
    {ok, _} = condor_listener_sup:start_child(SupName),
    start_acceptor_n(SupName, N - 1).

-spec listener_sup_name(atom()) -> atom().
listener_sup_name(Name) ->
    list_to_atom("condor_listener_sup_" ++ atom_to_list(Name)).
