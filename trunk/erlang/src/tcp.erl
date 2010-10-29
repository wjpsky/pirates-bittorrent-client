-module(tcp).

-export([listen/0, send/2, recv/2]).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

listen() ->
    {ok, LSocket} = gen_tcp:listen(6888, ?TCP_OPTIONS),
    io:format("Connected\n"),
    spawn(fun() -> accept(LSocket) end).
		 
accept(LSocket) ->
    {ok, Socket} = gen_tcp:accept(LSocket),   
    spawn(fun() -> loop(Socket) end),
    accept(LSocket).
  
send(Socket, Packet)->
    ok = gen_tcp:send(Socket, Packet),   
    loop(Packet),
    send(Socket, Packet).
		   
recv(Socket, Length) ->
    ok = gen_tcp:recv(Socket, Length),
    loop(Socket),
    recv(Socket, Length).

loop(Socket) ->
    case gen_tcp:recv(Socket, 100) of
        {ok, Packet} ->
	    gen_tcp:send(Socket, Packet),
            loop(Socket);
        {error, closed} ->
            io:format("Closed\n")
    end.
