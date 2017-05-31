-module(fstream_uploader_audio).

-behaviour(gen_server).

%% API.
-export([start_link/1
        ]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-include("deps/ffmpeg/include/ffmpeg.hrl").

-define(B2F(X), erlang:binary_to_float(X)).
-define(B2I(X), erlang:binary_to_integer(X)).
-define(TIMEOUT, 10000).


%% API.
%% -spec start_link([tuple()]) -> {ok, pid()}.
start_link(Args) ->
    lager:log(info, [], "starting with args ~p~n", [Args]),
    gen_server:start_link(?MODULE, Args, []).

%%
init(Args) ->
    {ok, State} = fstream_utils:init(Args),
    {_, FPath} = lists:keyfind(tmp_store_path, 1, Args),
    {ok, State#{tmp_store_path => FPath}}.


handle_call(Request, From, State) ->
    fstream_utils:handle_call(Request, From, State).


handle_cast({upload_part, Data}, #{user_args    := UArgs,
                                   caller       := CPid,
                                   part_handler := {M, F}} = State) ->
    %% lager:log(info, [], "UPLOADING: ~p~n", [size(Data)]),
    try  M:F(Data, UArgs) of
         {ok, UArgsNew} ->
            {noreply, State#{user_args := UArgsNew}, ?TIMEOUT}
    catch _:_ ->
            fstream_utils:inform_caller(CPid, {transcoding_status, {error, audio_local_store}}),
            {stop, abort, State}
    end;

handle_cast(Request, State) ->
    lager:log(info, [], "received unkown cast ~p~n", [Request]),
    {noreply, State}.

handle_info(Info, State) ->
    fstream_utils:handle_info(Info, State).

terminate(abort, State) ->
    fstream_utils:terminate(abort, State);

terminate(Reason, #{tmp_store_path := FilePath,
                    duration       := TDuration,
                    caller         := CPid} = State) ->
    FileInfo       = ffmpeg:infos(FilePath),
    BMediaDuration = FileInfo#ffmpeg_movie_info.format#ffmpeg_format_info.duration,
    MediaDuration  = ?B2F(BMediaDuration),
    lager:log(info, [], "TERMINATING: ~p~n", [MediaDuration]),

    case round(MediaDuration) >= trunc(TDuration) of
        true  -> fstream_utils:inform_caller(CPid, {transcoding_status, {ok, success}});
        false -> fstream_utils:inform_caller(CPid, {transcoding_status, {error, duration_mismatch}})
    end,
    fstream_utils:terminate(Reason, State).


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
