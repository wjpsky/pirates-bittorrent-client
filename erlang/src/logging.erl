 %%%-------------------------------------------------------------------
%%% @author Niel Madlani <nielmadlani@dhcp-164-144.nomad.chalmers.se>
%%% @copyright (C) 2010, Niel Madlani
%%% @doc
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
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init(_Args) ->
    {ok,[]}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

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

open(Append)->
    write(test3.txt,Append).

write(Pid, Append)->
    {ok, S} = file:open(Pid, [append, write]),  
    lists:foreach(fun(X) ->
    io:format(S, "~p.~n",[X]) end, Append), %Write to the file
    file:close(S).