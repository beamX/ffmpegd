-module(frecv_audio_protocol).
-behaviour(ranch_protocol).

-export([start_link/4]).
-export([init/4]).

start_link(Ref, Socket, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
    {ok, Pid}.

init(Ref, Socket, Transport, Opts) ->
    {forward_to, HandlerPid} = lists:keyfind(forward_to, 1, Opts),
    ok                       = ranch:accept_ack(Ref),
    loop(Socket, Transport, HandlerPid, <<>>).

loop(Socket, Transport, HandlerPid, _Acc) ->
    case Transport:recv(Socket, 0, infinity) of
        {ok, Data} ->
            fstream_utils:upload_part(HandlerPid, Data),
            loop(Socket, Transport, HandlerPid, _Acc);
        {error, closed} ->
            fstream_utils:stop(HandlerPid);
        Other ->
            lager:log(info, [], "UNKOWN: ~p~n", [Other])
    end.
