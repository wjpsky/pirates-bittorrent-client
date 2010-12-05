%% Author: nenastojova, jonkri
%% Created: Nov 2, 2010
%% Description: TODO: Add description to openFile
-module(open_file).
-behaviour(gen_server).
-export([handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3,init/1,start/0,stop/0]).
-export([open_file/1,start/1]).

-include_lib("kernel/include/file.hrl").


%%Starts the gen_server and registers the Pid to the event_manager
start()->
	case gen_server:start(?MODULE,[],[]) of
		{ok,Pid}->
			%event_manager:register(Pid),
			{ok,Pid};
		A->
			A
	end.
%%Unregisters the PId from the event_manager and stops the server
%stop(Pid)->
%	event_manager:unregister(Pid),
%	stop().
%%Stops the server but doesn't unregister the Pid
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
open_file(File) ->
    case file:open(File, [read]) of
		{ok, IoDevice} ->
	  		{ok, FileInfo} = file:read_file_info(File),
	  		case file:read(IoDevice, FileInfo#file_info.size) of
	    		{ok, Data} ->
					ParsedData = torrent_file_parser:decode(Data),
					%gen_server:cast(controller, {torrent_file_parsed, ParsedData});
					controller:parse_torrent_done(ParsedData);
	    		A ->
					A
	  		end;
      	{error, Reason} ->
	  		{File, error, Reason}
    end.

start(File)->
    case file:open(File, [read]) of
		{ok, IoDevice} ->
	  		{ok, FileInfo} = file:read_file_info(File),
	  		case file:read(IoDevice, FileInfo#file_info.size) of
	    		{ok, Data} ->
					event_manager:notify({torrent_file_content,Data});
					%ParsedData = torrent_file_parser:decode(Data),
					%gen_server:cast(controller, {torrent_file_parsed, ParsedData});
					%controller:parse_torrent_done(ParsedData);
	    		A ->
					A
	  		end;
      	{error, Reason} ->
	  		{File, error, Reason}
    end.

