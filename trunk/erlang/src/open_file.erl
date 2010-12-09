%% Author: nenastojova, jonkri
%% Created: Nov 2, 2010
%% Description: TODO: Add description to openFile
-module(open_file).
-behaviour(gen_server).
-export([handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3,init/1,start/0,stop/0]).
-export([start/1]).

-include_lib("kernel/include/file.hrl").


%%Starts the gen_server and registers the Pid to the event_manager
start()->
  gen_server:start(?MODULE,[],[]).
stop()->
	gen_server:cast(?MODULE,stop).

%% =============================================================================
%% EXPORTED GEN_SERVER CALLBACKS
%% =============================================================================

init(_Args)->
	{ok,[]}.

handle_call({parse_torrent_file, _File}, _From, State) ->
    {reply, ok, State}.

handle_cast({torrent_file_parsed, _ParsedData}, State) ->
    {noreply, State};

handle_cast(stop, State) -> {stop, normal, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.


%%Function that opens a file, and returns the data of the file 

start(File)->
    case file:open(File, [read]) of
		{ok, IoDevice} ->
	  		{ok, FileInfo} = file:read_file_info(File),
	  		case file:read(IoDevice, FileInfo#file_info.size) of
	    		{ok, Data} ->
					event_manager:notify({torrent_file_content,Data});
	    		A ->
					A
	  		end;
      	{error, Reason} ->
			event_manager:notify({open_torrent_file_error,Reason})
	  		%{File, error, Reason}
    end.

