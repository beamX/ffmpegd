-module(frecv_protocol).
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

loop(Socket, Transport, HandlerPid, Acc) ->
    case Transport:recv(Socket, 0, infinity) of
        {ok, Data} when Data =/= <<4>> ->
            if size(Data) >= 56 ->
                    case <<Data:56/bitstring>> of
                        <<"#EXTM3U">> ->
                            %% lager:log(info, [], "received data[~p, ~p] ~p~n", [self(), size(Acc), Data]),
                            fstream_uploader:upload_part(HandlerPid, Data),
                            loop(Socket, Transport, HandlerPid, <<>>);
                        _ ->
                            %% lager:log(info, [], "***************** received data[~p] ~p~n", [self(), size(Acc)]),
                            loop(Socket, Transport, HandlerPid, <<Acc/binary, Data/binary>>)
                    end;
               true ->
                    %% lager:log(info, [], "[less data][~p] ~p~n", [self(), Data]),
                    loop(Socket, Transport, HandlerPid, <<Acc/binary, Data/binary>>)
            end;

        _Other ->
            %% lager:log(info, [], " ============================ closing socket[~p] ~p~n", [self(), size(Acc)]),
            if size(Acc) > 0 -> fstream_uploader:recv_part(HandlerPid, Acc);
               true -> ok
            end
            %% ok = Transport:close(Socket)
            %% fstream_uploader:stop(HandlerPid)
    end.
