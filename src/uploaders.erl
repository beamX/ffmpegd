-module(uploaders).

-export([disk/4]).


disk(Name, Part, M3U8, State) ->
    ok = file:write_file(<<"/home/kansi/tmp/", Name/bitstring>>, Part),
    ok = file:write_file(<<"/home/kansi/tmp/feed1.m3u8">>, M3U8),
    {ok, State}.
