%% Authors: Niel Madlani, Navid Ranjbar, Jon Kristensen
%%
%% Description: This class connects to a port and sends and receives messages.
%%
%%              It implements the gen_server behaviour and thus defines a number
%%              of callbacks used by the gen_server module. See
%%              http://www.erlang.org/doc/man/gen_server.html for more
%%              information.
%%
%%              The state is the socket.

-module(tcp).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, start/0, stop/0, connect/0, send/1, recv/0]).


%% =============================================================================
%% EXPORTED GEN_SERVER CALLBACKS
%% =============================================================================

init([]) -> {ok, no_connection}.

handle_call({send, Packet}, _From, State) ->
	gen_tcp:send(State, Packet),
	io:format("Send Working\n"),
	{reply, ok, State};

handle_call(recv, _From, State) ->
	Message = gen_tcp:recv(State, 0),
    io:format("~w~n", [Message]),
	{reply, Message, State}.

handle_cast(connect, _) ->
    case gen_tcp:listen(6888, [list]) of
	{ok, LSocket}->
	    io:format("~w~n", [LSocket]),
	    case gen_tcp:accept(LSocket) of 
		{ok, Socket} ->
		    inet:setopts(Socket, [{active, false}]),
		    io:format("accepted\n"),
		    {noreply, Socket};
		Other ->
		    io:format("~w~n",[Other])
	    end;
	{error, Reason} ->
	    Reason
    end;

handle_cast(stop, State) -> {stop, normal, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.


%% =============================================================================
%% EXPORTED CONVENIENCE FUNCTIONS TO START AND STOP THE SERVER
%% =============================================================================

start() ->
	case gen_server:start({local, ?MODULE}, ?MODULE, [], []) of
		{ok, Pid} ->
			Pid;
		Reason ->
			Reason
	end.

stop() ->
	case gen_server:cast(?MODULE, stop) of
		ok ->
			ok;
		_ ->
			{error, stop_error}
	end.


%% =============================================================================
%% PUBLIC API FUNCTIONS
%% =============================================================================

connect() -> gen_server:cast(?MODULE, connect).

send(Packet) -> gen_server:call(?MODULE, {send, Packet}).

recv() -> gen_server:call(?MODULE, recv).


%% =============================================================================
%% LOCAL FUNCTIONS
%% =============================================================================