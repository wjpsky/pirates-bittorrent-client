%% Authors: Jon Kristensen
%%
%% Description: This class is the target of events from for example a GUI. It
%%              makes things happen in the application.
%%
%%              It implements the gen_server behaviour and thus defines a number
%%              of callbacks used by the gen_server module. See
%%              http://www.erlang.org/doc/man/gen_server.html for more
%%              information.
%%
%%              We need to start this process from a erl node started with "erl
%%              -sname 'erlangpbtc@localhost'".

-module(controller).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, start/0, stop/0, parse_torrent_file/1]).


%% =============================================================================
%% EXPORTED GEN_SERVER CALLBACKS
%% =============================================================================

init([]) -> {ok, {}}.

handle_call({parse_torrent_file, File}, _From, State) ->
	spawn(openFile, start, [File]),
	{reply, ok, State}.

handle_cast({torrent_file_parsed, ParsedData}, State) ->
	io:format("Torrent file parsed.\n"),
	Record = fileRecords:toRec(ParsedData),
	fileRecords:test(Record),
	%% io:format("~p.\n", [Record]),
	%% spawn(tracker, start, [ParsedData]),
	{noreply, State};

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
		{error, Reason} ->
			error_logger:error_report("An error occurred", Reason, [?LINE,?MODULE])
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

parse_torrent_file(File) ->
	gen_server:call(?MODULE, {parse_torrent_file, File}).


%% =============================================================================
%% LOCAL FUNCTIONS
%% =============================================================================
