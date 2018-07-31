-module(condor_listener).
-behaviour(gen_server).

%% -----------------------------------------------------------------------------
%% API
%% -----------------------------------------------------------------------------
-export([start_link/4]).
-export([accept/1]).
-export([stop/1]).

%% -----------------------------------------------------------------------------
%% gen_server callbacks
%% -----------------------------------------------------------------------------
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

%% -----------------------------------------------------------------------------
%% condor_listener behaviour
%% -----------------------------------------------------------------------------
-type sock() :: inet:socket().
-type packet() :: binary().
-type msg() :: term().
-type state() :: term().
-type terminate_reason() :: term().

-type result() :: {ok, state()}
                | {send, packet(), state()}
                | {stop, terminate_reason(), state()}.

-callback init(state()) ->
    result().

-callback handle_packet(packet(), state()) ->
    result().

-callback handle_info(msg(), state()) ->
    result().

-callback terminate(terminate_reason(), state()) ->
    ok.

%% -----------------------------------------------------------------------------

-record(state, {mod :: module(),
                mod_init_state :: term(),
                mod_state :: term(),
                lsock :: undefined | sock(),
                sock :: undefined | sock(),
                len = 16 :: 8 | 16 | 32,
                buffer = <<>> :: binary()}).

%% -----------------------------------------------------------------------------
%% API
%% -----------------------------------------------------------------------------
start_link(LSock, Mod, ModInitState, Opts) ->
    gen_server:start_link(?MODULE, [LSock, Mod, ModInitState, Opts], []).

accept(Pid) ->
    {ok, _} = timer:apply_after(0, gen_server, cast, [Pid, accept]),
    ok.

stop(Pid) ->
    gen_server:cast(Pid, stop).

%% -----------------------------------------------------------------------------
%% gen_server callbacks
%% -----------------------------------------------------------------------------
init([LSock, Mod, ModInitState, #{len:=Len}]) ->
    process_flag(trap_exit, true),
    accept(self()),
    State = #state{mod = Mod,
                   mod_init_state = ModInitState,
                   lsock = LSock,
                   len = Len * 8},
    {ok, State}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(accept,
            #state{mod=Mod, mod_init_state=ModInitState, lsock=LSock}=State) ->
    %% init/1 callback
    catch gen_tcp:shutdown(State#state.sock, read_write),
    {ok, Sock} = gen_tcp:accept(LSock),
    NewState = handle_callback(Mod, init, [ModInitState], State#state{sock=Sock}),
    {noreply, NewState};
handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info({tcp, Sock, Data},
            #state{len=Len, buffer=Buf,
                   sock=Sock, mod=Mod, mod_state=ModState}=State) ->
    NewBuf = <<Buf/binary, Data/binary>>,
    NewState = case condor_packet:decode(Len, NewBuf) of
                   {ok, Packet, RestBuf} ->
                       %% handle_packet/3 callback
                       handle_callback(
                         Mod,
                         handle_packet,
                         [Packet, ModState],
                         State#state{buffer = RestBuf}
                        );
                   {more, _} ->
                       State#state{buffer = NewBuf}
               end,
    ok = inet:setopts(Sock, [{active, once}]),
    {noreply, NewState};
handle_info({tcp_error, Reason, Sock},
            #state{sock=Sock, mod=Mod, mod_state=ModState}=State) ->
    %% terminate/2 callback and reuse the process
    accept(self()),
    NewState = handle_callback(Mod, terminate, [Reason, ModState], State),
    {noreply, NewState};
handle_info({tcp_closed, Sock},
            #state{sock=Sock, mod=Mod, mod_state=ModState}=State) ->
    %% terminate/2 callback and reuse the process
    accept(self()),
    NewState = handle_callback(Mod, terminate, [tcp_closed, ModState], State),
    {noreply, NewState};
handle_info(Msg, #state{mod=Mod, mod_state=ModState}=State) ->
    %% handle_info/2 callback
    NewState = handle_callback(Mod, handle_info, [Msg, ModState], State),
    {noreply, NewState}.

terminate(_Reason, #state{lsock=LSock}) ->
    catch gen_tcp:shutdown(LSock, read_write),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% -----------------------------------------------------------------------------
%% internal
%% -----------------------------------------------------------------------------
handle_callback(M, F, A, State) ->
    case (catch apply(M, F, A)) of
        {'EXIT', Trace} ->
            error_logger:error_msg("~p~n", [Trace]),
            State;
        ok ->
            State#state{mod_state = undefined, buffer = <<>>};
        {ok, ModState} ->
            State#state{mod_state = ModState};
        {send, Data, ModState} ->
            send(Data, State),
            State#state{mod_state = ModState};
        {stop, Reason, ModState} ->
            %% terminate/2 callback and reuse the process
            accept(self()),
            handle_callback(M, terminate, [Reason, ModState], State);
        Else ->
            error_logger:error_msg("** Bad return value == ~p~n", [Else]),
            State
    end.

send(Data0, #state{len=L, sock=Sock}) ->
    Data = condor_packet:encode(L, Data0),
    gen_tcp:send(Sock, Data).
