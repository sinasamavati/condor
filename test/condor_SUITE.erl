-module(condor_SUITE).
-behaviour(condor_listener).

-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([all/0]).
-export([echo_test/1]).
-export([timeout_test/1]).

-include_lib("common_test/include/ct.hrl").

init_per_suite(Config) ->
    ok = application:start(condor),
    Port = rand_port(),
    Opts = #{port => Port, max_acceptors => 5, len => 2},
    {ok, _} = condor:start_listener(echo_server, Opts, ?MODULE, []),
    [{port, Port} | Config].

end_per_suite(_Config) ->
    ok = condor:stop_listener(echo_server),
    ok.

all() ->
    [
     condor_packet,
     echo_test,
     timeout_test
    ].

condor_packet(_) ->
    <<1, "a">> = condor_packet:encode(8, <<"a">>),
    <<0, 3, "aaa">> = condor_packet:encode(16, <<"aaa">>),
    {ok, <<"a">>, <<"aa">>} = condor_packet:decode(16, <<0, 1, "aaa">>),
    {ok, <<"aaa">>, <<>>} = condor_packet:decode(16, <<0, 3, "aaa">>),

    Data = binary:copy(<<"A">>, 1024),
    Packet = condor_packet:encode(16, Data),
    {ok, Data, <<>>} = condor_packet:decode(16, Packet),
    ok.

echo_test(Config) ->
    Port = ?config(port, Config),

    {ok, Sock0} = gen_tcp:connect("localhost", Port, [binary]),
    Pkt0 = condor_packet:encode(16, binary:copy(<<"A">>, 1024)),
    ok = gen_tcp:send(Sock0, Pkt0),
    Pkt0 = loop_recv(Sock0),

    {ok, Sock1} = gen_tcp:connect("localhost", Port, [binary]),
    Pkt1 = condor_packet:encode(16, binary:copy(<<"ABC">>, 10240)),
    ok = gen_tcp:send(Sock1, Pkt1),
    Pkt1 = loop_recv(Sock1),

    {ok, Sock2} = gen_tcp:connect("localhost", Port, [binary]),
    Pkt2 = condor_packet:encode(16, <<"send_and_stop">>),
    ok = gen_tcp:send(Sock2, Pkt2),
    Pkt2 = loop_recv(Sock2),
    receive
        {tcp_closed, Sock2} ->
            ok
    after 10 ->
            exit(tcp_not_closed)
    end,

    ok = gen_tcp:close(Sock0),
    ok = gen_tcp:close(Sock1),
    ok.

timeout_test(_Config) ->
    ok = application:ensure_started(condor),
    Port = rand_port(),
    Opts = #{port => Port, len => 2, timeout => 20, max_acceptors => 5},
    {ok, _} = condor:start_listener(timeout_server, Opts, ?MODULE, []),

    %% let timeout trigger by not sending the entire packet
    {ok, Sock0} = gen_tcp:connect("localhost", Port, [binary]),
    <<Pkt0:64/binary, _/binary>> = condor_packet:encode(16, binary:copy(<<"A">>, 128)),
    ok = gen_tcp:send(Sock0, Pkt0),
    receive
        {tcp, Sock0, <<_:16, Pkt0/binary>>} ->
            ok
    after 30 ->
            exit(not_received)
    end,

    %% send two chunks of packet, and let the timeout trigger
    {ok, Sock1} = gen_tcp:connect("localhost", Port, [binary]),
    <<Pkt1:64/binary, Pkt2:64/binary, _/binary>> =
        condor_packet:encode(16, binary:copy(<<"XYZ">>, 256)),
    ok = gen_tcp:send(Sock1, Pkt1),
    ok = timer:sleep(10),
    ok = gen_tcp:send(Sock1, Pkt2),
    receive
        {tcp, Sock1, <<_:16, Pkt1:64/binary, Pkt2:64/binary>>} ->
            ok
    after 30 ->
            exit(not_received)
    end,

    %% send the entire packet in three chunks, and do not trigger timeout
    {ok, Sock2} = gen_tcp:connect("localhost", Port, [binary]),
    Pkt3 = condor_packet:encode(16, binary:copy(<<"XYZ">>, 256)),
    ok = gen_tcp:send(Sock2, binary:part(Pkt3, 0, 64)),
    ok = gen_tcp:send(Sock2, binary:part(Pkt3, 64, 64)),
    ok = gen_tcp:send(Sock2, binary:part(Pkt3, 128, byte_size(Pkt3) - 128)),
    receive
        {tcp, Sock2, Pkt3} ->
            ok
    after 30 ->
            exit(not_received)
    end,

    %% send the entire packet in three chunks with a bit of pause in between.
    %% this will trigger timeout, because the packet won't be sent completely
    %% in 20 milliseconds
    ok = gen_tcp:send(Sock2, binary:part(Pkt3, 0, 64)),
    ok = timer:sleep(10),
    ok = gen_tcp:send(Sock2, binary:part(Pkt3, 64, 64)),
    ok = timer:sleep(10),
    ok = gen_tcp:send(Sock2, binary:part(Pkt3, 128, byte_size(Pkt3) - 128)),
    receive
        {tcp, Sock2, Pkt3} ->
            exit(should_not_receive_full_packet);
        {tcp, Sock2, _Buf} ->
            ok
    after 30 ->
            exit(not_received)
    end,

    ok = gen_tcp:close(Sock0),
    ok = gen_tcp:close(Sock1),
    ok = gen_tcp:close(Sock2),
    ok = condor:stop_listener(timeout_server),
    ok.

%% -----------------------------------------------------------------------------
%% internal
%% -----------------------------------------------------------------------------
rand_port() ->
    erlang:system_time() rem 10000.

loop_recv(Sock) ->
    loop_recv(Sock, <<>>).

loop_recv(Sock, Buffer) ->
    receive
        {tcp, Sock, Pkt} ->
            loop_recv(Sock, <<Buffer/binary, Pkt/binary>>)
    after 10 ->
            Buffer
    end.

%% -----------------------------------------------------------------------------
%% condor_listener callbacks
%% -----------------------------------------------------------------------------
init([]) ->
    {ok, undefined}.

handle_packet(<<"send_and_stop">> = Data, State) ->
    {send_and_stop, Data, normal, State};
handle_packet(Echo, State) ->
    {send, Echo, State}.

handle_info({timeout, Buf}, State) ->
    {send, Buf, State};
handle_info(_MSg, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
