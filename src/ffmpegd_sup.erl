-module(ffmpegd_sup).
-behaviour(supervisor).

-export([start_link/0,
         start_s3_child/1]).

-export([init/1]).

-include("rffmpeg.hrl").

-define(CHILD(Id, Mod, Args, Restart, Type), #{id => Id,
                                               start => {Mod, start_link, Args},
                                               restart => Restart,
                                               shutdown => 5,
                                               type => Type,
                                               modules => [Mod]}).

-define(SIMPLE_CHILD(Id, WorkerMod), ?CHILD(Id, WorkerMod, [[]], transient,
                                            worker)).

-define(SIMPLE_SUP(SupId, WorkerMod),
        ?CHILD(SupId, simple_sup,
               [SupId, simple_one_for_one, [?CHILD(WorkerMod, WorkerMod, [], transient, worker) ]], permanent,
               supervisor)).

%% API.
start_s3_child(Duration) ->
    Args = [{duration, Duration},
            {part_handler, {uploaders, disk}},
            {user_args, []} ],
    supervisor:start_child(fstream_uploader_sup, [Args]).


-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init([]) ->
    %% S3Uploaders = lists:map(fun(Port) ->
    %%                                 ?SIMPLE_CHILD(Port, fstream_uploader)
    %%                         end, ?PORTS),
    Processes = [?SIMPLE_SUP(fstream_uploader_sup, fstream_uploader),
                 ?SIMPLE_CHILD(port_manager, port_manager)
                ],
    {ok, {{one_for_one, 10, 10}, Processes}}.
