-module(fstream_utils).

-export([get_port/1,
         recv_part/2,
         upload_part/2,
         stop/1
        ]).

-export([init/1,
         handle_call/3,
         handle_info/2,
         terminate/2,
         inform_caller/2
        ]).

-define(B2F(X), erlang:binary_to_float(X)).
-define(B2I(X), erlang:binary_to_integer(X)).


get_port(Pid) ->
    gen_server:call(Pid, {get_port}).

recv_part(Pid, Bin) ->
    gen_server:cast(Pid, {recv_part, Bin}).

upload_part(Pid, Data) ->
    gen_server:cast(Pid, {upload_part, Data}).

stop(Pid) ->
    gen_server:call(Pid, {stop}).





init(Args) ->
    process_flag(trap_exit, true),

    {_, Duration} = lists:keyfind(duration, 1, Args),
    {_, MF}       = lists:keyfind(part_handler, 1, Args),
    {_, UArgs}    = lists:keyfind(user_args, 1, Args),
    {_, Caller}   = lists:keyfind(caller, 1, Args),
    {_, Protocol} = lists:keyfind(protocol, 1, Args),

    {ok, Port}    = port_manager:get_free_port(self()),
    lager:log(info, [], "got port ~p~n", [Port]),
    %% Port       = 10009,
    BPort         = erlang:integer_to_binary(Port),
    lager:log(info, [], "PORT: ~p~n", [Port]),
    %% {ok, _}       = ranch:start_listener(BPort, 1, ranch_tcp, [{port, Port}],
    %%                                      frecv_protocol, [{forward_to, self()}]),
    {ok, _}       = ranch:start_listener(BPort, 1, ranch_tcp, [{port, Port}],
                                         Protocol, [{forward_to, self()}]),
    {ok, #{socket       => BPort,
           parts        => [],
           tot_duration => Duration,
           duration     => 0.0,
           part_handler => MF,
           user_args    => UArgs,
           caller       => Caller
          }}.


%% common handle_call's
handle_call({get_port}, _From, #{socket := Port} = State) ->
    {reply, {ok, ?B2I(Port)}, State};

handle_call({stop}, _From, State) ->
    {stop, normal, State};

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.


handle_info(timeout, #{caller := CPid} = State) ->
    lager:log(error, [], "fstream_uploader timeout while decoding ~p~n", [State]),
    inform_caller(CPid, {transcoding_status, {error, timeout}}),
    {stop, normal, State};

handle_info(Info, State) ->
    lager:log(info, [], "~p received unkown message ~p~n", [?MODULE, Info]),
    {noreply, State}.


inform_caller(CPid, Msg) ->
    CPid ! Msg.


terminate(Reason, #{socket := BPort} = _State) ->
    lager:log(info, [], "=================== shutting down ~p ~p ~n", [BPort, Reason]),
    ranch:stop_listener(BPort),
    port_manager:put_free_port(?B2I(BPort)).
