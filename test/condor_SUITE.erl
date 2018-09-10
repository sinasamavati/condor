-module(condor_SUITE).
-behaviour(condor_listener).

-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([all/0]).
-export([echo_test/1]).

-include_lib("common_test/include/ct.hrl").

init_per_suite(Config) ->
    ok = application:start(condor),
    Port = crypto:rand_uniform(3000, 9999),
    Opts = #{port => Port, max_acceptors => 5, len => 2},
    {ok, _} = condor:start_listener(echo_server, Opts, ?MODULE, []),
    [{port, Port} | Config].

end_per_suite(_Config) ->
    ok = condor:stop_listener(echo_server),
    ok.

all() ->
    [condor_packet, echo_test].

condor_packet(_) ->
    <<1, "a">> = condor_packet:encode(8, <<"a">>),
    <<0, 3, "aaa">> = condor_packet:encode(16, <<"aaa">>),
    {ok, <<"a">>, <<"aa">>} = condor_packet:decode(16, <<0, 1, "aaa">>),
    {ok, <<"aaa">>, <<>>} = condor_packet:decode(16, <<0, 3, "aaa">>),

    Data = iolist_to_binary(["A" || _ <- lists:seq(1, 1024)]),
    Packet = condor_packet:encode(16, Data),
    {ok, Data, <<>>} = condor_packet:decode(16, Packet),
    ok.

echo_test(Config) ->
    Port = ?config(port, Config),

    {ok, Sock0} = gen_tcp:connect("localhost", Port, [binary]),
    Pkt0 = condor_packet:encode(16, ["A" || _ <- lists:seq(1, 1024)]),
    gen_tcp:send(Sock0, Pkt0),
    Pkt0 = loop_recv(Sock0),

    {ok, Sock1} = gen_tcp:connect("localhost", Port, [binary]),
    Pkt1 = condor_packet:encode(16, ["ABC" || _ <- lists:seq(1, 10240)]),
    gen_tcp:send(Sock1, Pkt1),
    Pkt1 = loop_recv(Sock1),

    {ok, Sock2} = gen_tcp:connect("localhost", Port, [binary]),
    Pkt2 = condor_packet:encode(16, <<"send_and_stop">>),
    gen_tcp:send(Sock2, Pkt2),
    Pkt2 = loop_recv(Sock2),
    receive
        {tcp_closed, Sock2} ->
            ok
    after 10 ->
            exit(tcp_not_closed)
    end,

    gen_tcp:close(Sock0),
    gen_tcp:close(Sock1),
    ok.

%% -----------------------------------------------------------------------------
%% internal
%% -----------------------------------------------------------------------------
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

handle_info(_MSg, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
