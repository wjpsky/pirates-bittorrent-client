%% Author: nenastojova, jonkri
%% Created: Nov 2, 2010
%% Description: TODO: Add description to openFile
-module(open_file).
-export([start/1]).
-include_lib("kernel/include/file.hrl").

%%Function that opens a file, and returns the data of the file 
start(File) ->
    case file:open(File, [read]) of
      {ok, IoDevice} ->
	  {ok, FileInfo} = file:read_file_info(File),
	  case file:read(IoDevice, FileInfo#file_info.size) of
	    {ok, Data} ->
		ParsedData = torrent_file_parser:decode(Data),
		gen_server:cast(controller, {torrent_file_parsed, ParsedData});
	    A -> A
	  end;
      {error, Reason} ->
	  {File, error, Reason}
    end.