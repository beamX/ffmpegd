-module(simple_sup).

-behaviour(supervisor).

%% API functions
-export([start_link/3]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).


%%%===================================================================
%%% API functions
%%%===================================================================
start_link(Name, Type, ChildSpecs) ->
    supervisor:start_link({local, Name}, ?MODULE, [Type, ChildSpecs]).


init([Type, ChildSpecs]) ->
    {ok, {{Type, 5, 10}, ChildSpecs}}.
    %{ok, {{simple_one_for_one, 5, 10}, [CHILD]}}.
