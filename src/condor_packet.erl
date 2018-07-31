-module(condor_packet).

-export([encode/2]).
-export([decode/2]).

-spec encode(non_neg_integer(), iolist()) -> binary().
encode(L, Data) ->
    Bin = iolist_to_binary(Data),
    Len = byte_size(Bin),
    <<Len:L/unsigned-big, Bin/binary>>.

-spec decode(non_neg_integer(), binary()) ->
                    {ok, binary(), binary()} |
                    {more, non_neg_integer()} |
                    {error, term()}.
decode(Len, Bin) ->
    erlang:decode_packet(trunc(Len / 8), Bin, []).
