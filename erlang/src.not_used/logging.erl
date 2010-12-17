 %%%-------------------------------------------------------------------
%%% @author Niel Madlani <nielmadlani@dhcp-164-144.nomad.chalmers.se>, JianPing Wang, Nena Stojova
%%% @copyright (C) 2010, Niel Madlani
%%% @doc
%%% The logging module will receive event from the event_manager. When it recives the event
%%%	it will store them to the file that is created.
%%%	
%%%	NOTE: The txt file will only be created one time. So everytime you start the program it wont make
%%		  any duplicate txt files. 
%%%
%%% @end
%%% Created :  7 Dec 2010 by Niel Madlani <nielmadlani@dhcp-164-144.nomad.chalmers.se>
%%%-------------------------------------------------------------------
-module(logging).

-behaviour(gen_server).

%% API
-export([start/0, open/1, write/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).


%%%===================================================================
%%% API
%%%===================================================================
start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init(_Args) ->
	event_manager:register(self()),
    {ok,[]}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({notify_event,Event},State)->
	{noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%Will create a new file
open(Append)->
    write(logMessage.txt,Append).

%%Write to the txt file 
write(Pid, Append)->
    {ok, S} = file:open(Pid, [append, write]),  
    lists:foreach(fun(X) ->
    io:format(S, "~p.~n",[X]) end, Append), %Write to the file
    file:close(S).