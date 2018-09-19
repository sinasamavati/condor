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
-type msg() :: {timeout, binary()} | term().
-type state() :: term().
-type terminate_reason() :: term().

-type result() :: {ok, state()}
                | {send, packet(), state()}
                | {stop, terminate_reason(), state()}
                | {send_and_stop, packet(), terminate_reason(), state()}.

-callback init(state()) ->
    result().

-callback handle_packet(packet(), state()) ->
    result().

-callback handle_info(msg(), state()) ->
    result().

-callback terminate(terminate_reason(), state()) ->
    ok.

%% -----------------------------------------------------------------------------

-record(state, {
          mod :: module(),
          mod_init_state :: term(),
          mod_state :: term(),
          lsock :: undefined | sock(),
          sock :: undefined | sock(),
          len = 16 :: 8 | 16,
          buffer = <<>> :: binary(),
          timeout = 10000 :: integer(),
          timer_ref :: undefined | reference()
         }).

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
init([LSock, Mod, ModInitState, #{len:=Len, timeout:=Timeout}]) ->
    process_flag(trap_exit, true),
    accept(self()),
    State = #state{
               mod = Mod,
               mod_init_state = ModInitState,
               lsock = LSock,
               len = Len * 8,
               timeout = Timeout
              },
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
                   sock=Sock, mod=Mod, mod_state=ModState}=State0) ->
    %% set a timeout for receiving the packet, if no timeout is set already
    State = maybe_set_timeout(State0),

    NewBuf = <<Buf/binary, Data/binary>>,
    NewState =
        case condor_packet:decode(Len, NewBuf) of
            {ok, Packet, RestBuf} ->
                State1 = cancel_timeout(State),
                %% handle_packet/3 callback
                handle_callback(
                  Mod,
                  handle_packet,
                  [Packet, ModState],
                  State1#state{buffer = RestBuf}
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
handle_info({timeout, _, packet_timeout},
            #state{mod=Mod, mod_state=ModState, buffer=Buf}=State) ->
    NewState =
        handle_callback(Mod, handle_info, [{timeout, Buf}, ModState], State),
    {noreply, NewState#state{buffer = <<>>, timer_ref = undefined}};
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
        {send_and_stop, Data, Reason, ModState} ->
            %% send data, then terminate/2 callback and reuse the process
            send(Data, State),
            accept(self()),
            handle_callback(M, terminate, [Reason, ModState], State);
        Else ->
            error_logger:error_msg("** Bad return value == ~p~n", [Else]),
            State
    end.

send(Data0, #state{len=L, sock=Sock}) ->
    Data = condor_packet:encode(L, Data0),
    gen_tcp:send(Sock, Data).

maybe_set_timeout(#state{timer_ref=undefined, timeout=Timeout}=State) ->
    TimerRef = erlang:start_timer(Timeout, self(), packet_timeout),
    State#state{timer_ref = TimerRef};
maybe_set_timeout(State) ->
    State.

cancel_timeout(#state{timer_ref=TimerRef}=State) ->
    catch erlang:cancel_timer(TimerRef),
    State#state{timer_ref = undefined}.
