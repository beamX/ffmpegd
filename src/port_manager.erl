-module(port_manager).

-behaviour(gen_server).

%% API.
-export([start_link/1,
         get_free_port/1,
         put_free_port/1
        ]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).



%% API.
get_free_port(_Pid) ->
    gen_server:call(?MODULE, {get_free_port}).

put_free_port(Port) ->
    gen_server:call(?MODULE, {put_free_port, Port}).


-spec start_link([tuple()]) -> {ok, pid()}.
start_link(_Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%
init(_Args) ->
    {ok, #{ports => lists:seq(20001, 20020)}}.

handle_call({get_free_port}, _From, #{ports := []} = State) ->
    {reply, {error, none}, State};

handle_call({get_free_port}, _From, #{ports := Ports} = State) ->
    [OpenPort | Rest] = Ports,
    {reply, {ok, OpenPort}, State#{ports => Rest}};

handle_call({put_free_port, Port}, _From, #{ports := Ports} = State) ->
    {reply, ok, State#{ports => lists:reverse([Port | Ports])} };

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.


handle_cast(Request, State) ->
    lager:log(info, [], "received unkown cast ~p~n", [Request]),
    {noreply, State}.


handle_info(Info, State) ->
    lager:log(info, [], "~p received unkown message ~p~n", [?MODULE, Info]),
    {noreply, State}.

terminate(_Reason, #{socket := Socket} = _State) ->
    lager:log(info, [], "shutting down ~p~n", [Socket]),
    ranch:stop_listener(Socket).


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

