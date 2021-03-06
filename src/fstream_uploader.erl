-module(fstream_uploader).

-behaviour(gen_server).

%% API.
-export([start_link/1,
         get_port/1,
         recv_part/2,
         upload_part/2,
         get_segment_name/1,
         stop/1
        ]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-define(B2F(X), erlang:binary_to_float(X)).
-define(B2I(X), erlang:binary_to_integer(X)).
-define(TIMEOUT, 10000).


get_port(Pid) ->
    gen_server:call(Pid, {get_port}).

recv_part(Pid, Bin) ->
    gen_server:cast(Pid, {recv_part, Bin}).

upload_part(Pid, MetaData) ->
    gen_server:cast(Pid, {upload_part, MetaData}).

stop(Pid) ->
    gen_server:call(Pid, {stop}).


%% API.
%% -spec start_link([tuple()]) -> {ok, pid()}.
start_link(Args) ->
    lager:log(info, [], "starting with args ~p~n", [Args]),
    gen_server:start_link(?MODULE, Args, []).
    %% gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

%%
init(Args) ->
    fstream_utils:init(Args).
    %% process_flag(trap_exit, true),

    %% {_, Duration} = lists:keyfind(duration, 1, Args),
    %% {_, MF}       = lists:keyfind(part_handler, 1, Args),
    %% {_, UArgs}    = lists:keyfind(user_args, 1, Args),
    %% {_, Caller}   = lists:keyfind(caller, 1, Args),
    %% {_, Protocol} = lists:keyfind(protocol, 1, Args),

    %% {ok, Port}    = port_manager:get_free_port(self()),
    %% lager:log(info, [], "got port ~p~n", [Port]),
    %% %% Port       = 10009,
    %% BPort         = erlang:integer_to_binary(Port),
    %% lager:log(info, [], "PORT: ~p~n", [Port]),
    %% %% {ok, _}       = ranch:start_listener(BPort, 1, ranch_tcp, [{port, Port}],
    %% %%                                      frecv_protocol, [{forward_to, self()}]),
    %% {ok, _}       = ranch:start_listener(BPort, 1, ranch_tcp, [{port, Port}],
    %%                                      Protocol, [{forward_to, self()}]),
    %% {ok, #{socket       => BPort,
    %%        parts        => [],
    %%        tot_duration => Duration,
    %%        duration     => 0.0,
    %%        part_handler => MF,
    %%        user_args    => UArgs,
    %%        caller       => Caller
    %%       }}.

%% handle_call({get_port}, _From, #{socket := Port} = State) ->
%%     {reply, {ok, ?B2I(Port)}, State};

%% handle_call({stop}, _From, State) ->
%%     {stop, normal, State};

%% handle_call(_Request, _From, State) ->
%%     {reply, ignored, State}.

handle_call(Request, From, State) ->
    fstream_utils:handle_call(Request, From, State).

handle_cast({upload_part, MetaData}, #{parts := []} = State) ->
    lager:log(info, [], "--------------- no parts to upload ~p~n", [MetaData]),
    {noreply, State, ?TIMEOUT};

handle_cast({upload_part, MetaData}, #{parts        := Parts,
                                       tot_duration := TDuration,
                                       duration     := Duration,
                                       part_handler := {M, F},
                                       caller       := CPid,
                                       user_args    := UArgs} = State) ->
    [Part | Rest]       = lists:reverse(Parts),
    RParts              = lists:reverse(Rest),
    {Name, SegDuration} = get_segment_name(MetaData),

    try M:F(Name, Part, MetaData, UArgs) of
        {ok, UArgsNew} ->

            NewDuration = Duration + SegDuration,
            lager:log(info, [], "Duration: ~p ~p ~n", [NewDuration, TDuration]),
            if NewDuration =:= TDuration ->
                    {stop, normal, State};
               %% NOTE: careful of this condition
               round(NewDuration) >= trunc(TDuration) ->
                    fstream_utils:inform_caller(CPid, {transcoding_status, {ok, success}}),
                    {stop, normal, State};

               true ->
                    {noreply, State#{parts     := RParts,
                                     duration  := Duration + SegDuration,
                                     user_args := UArgsNew }, ?TIMEOUT}
            end
    catch Type:Error ->
            lager:log(info, [], "~p:~p~n", [Type, Error]),
            fstream_utils:inform_caller(CPid, {transcoding_status, {error, video_s3_upload}}),
            {stop, normal, State}
    end;


handle_cast({recv_part, Bin}, #{parts := Parts} = State) ->
    Parts1 = [Bin | Parts],
    {noreply, State#{parts => Parts1}, ?TIMEOUT};
handle_cast(Request, State) ->
    lager:log(info, [], "received unkown cast ~p~n", [Request]),
    {noreply, State}.


%% handle_info(timeout, #{caller := CPid} = State) ->
%%     lager:log(error, [], "fstream_uploader timeout while decoding ~p~n", [State]),
%%     inform_caller(CPid, {transcoding_status, {error, timeout}}),
%%     {stop, normal, State};
%% handle_info(Info, State) ->
%%     lager:log(info, [], "~p received unkown message ~p~n", [?MODULE, Info]),
%%     {noreply, State}.

handle_info(Info, State) ->
    fstream_utils:handle_info(Info, State).


terminate(Reason, State) ->
    fstream_utils:terminate(Reason, State).
%% terminate(Reason, #{socket := BPort} = _State) ->
%%     lager:log(info, [], "=================== shutting down ~p ~p ~n", [BPort, Reason]),
%%     ranch:stop_listener(BPort),
%%     port_manager:put_free_port(?B2I(BPort)).


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



get_segment_name(M3U8) ->
    [_, _, _, _ | PartNames] = binary:split(M3U8, [<<"\n">>], [global]),
    hd(get_names(PartNames, [])).

get_names([], Acc) -> Acc;
get_names([_], Acc) -> Acc;
get_names([Duration, Name | Rest], Acc) ->
    try <<Duration:56/bitstring>> of
        <<"#EXTINF">> ->
            [_, BDuration, _] = binary:split(Duration, [<<",">>, <<":">>], [global]),
            get_names(Rest, [{Name, ?B2F(BDuration)} | Acc]);
        _ ->
            get_names(Rest, Acc)
    catch _:_ ->
            lager:log(info, [], "error while decoding part name"),
            get_names(Rest, Acc)
    end.


%% inform_caller(CPid, Msg) ->
%%     CPid ! Msg.
