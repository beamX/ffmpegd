-module(uploaders).

-export([disk/4,
         disk_audio/2]).


disk(Name, Part, M3U8, State) ->
    ok = file:write_file(<<"/tmp/", Name/bitstring>>, Part),
    ok = file:write_file(<<"/tmp/feed1.m3u8">>, M3U8),
    {ok, State}.


disk_audio(Data, #{tmp_store_path := FilePath} = State) ->
    lager:log(info, [], "SAVING: ~p~n", [size(Data)]),
    %% ok = file:write_file(<<"/tmp/test.mp3">>, Data, [append]),
    ok = file:write_file(FilePath, Data, [append]),
    {ok, State}.
