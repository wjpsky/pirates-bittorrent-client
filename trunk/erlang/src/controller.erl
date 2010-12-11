%%==============================================================================
%% MODULE DESCRIPTION
%%==============================================================================

% @author Jon Kristensen
%     (did everything but the notify_event messages)
% @author Nena Stojova
%     (implemented the notify_event messages)

% @doc
% <p>The controller process is a gen_server which receives events and acts on
% them. These messages can come from both the Java GUI and other modules. The
% most important message right now is probably the "RPC" function callable from
% the Java code, "parse_torrent_file(File)". The other messages are regular
% Erlang messages, such as {parse_torrent_file, File}, etc.</p>
% <p>We need to start this process from a erl node started with <code>erl -sname
% 'erlangpbtc@localhost'</code> in order for it to be able to communicate with
% the Java code.</p>
% <p>Below are instructions on how to run this code.</p>
% <ol>
% <li><code>application:start(sasl).</code></li>
% <li><code>l(controller).</code></li>
% <li><code>l(open_file).</code></li>
% <li><code>l(torrent_file_parser).</code></li>
% <li><code>controller:start().</code></li>
% <li><code>controller:parse_torrent_file([INSERT FILE NAME]).</code></li>
% </ol>
% @end

%%==============================================================================
%% MODULE HEADER
%%==============================================================================

-module(controller).

-include("torrent_records.hrl").

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, start/0, stop/0, parse_torrent_file/1]).

%%==============================================================================
%% GEN_SERVER CALLBACKS
%%==============================================================================

% Initializes the gen_server, generating a peer ID for this client.
init([]) -> 
	{ok, peer_id:get_id()}.

% Handles the parse_torrent_file message. This message is sent from the
% parse_torrent_file function below. It spawns an open_file process.
handle_call({parse_torrent_file, File}, _From, State) ->
    spawn(open_file, start, [File]),
    {reply, ok, State}.

% Handles the torrent_file_parsed message. This message is sent from the
% open_file module. It converts the parsed torrent data into records and spawns
% the tracker process to begin communicating with the tracker.
handle_cast({torrent_file_parsed, ParsedData}, State) ->
    io:format("Torrent file parsed.\n"),
    Record = file_records:toRec(ParsedData),
    spawn(tracker, start, [Record, State]),
	{noreply, State};

% Handles the got_peers message sent from the tracker module. It spawns a
% download_fsm process to start downloading.
handle_cast({got_peers, Peers, Pieces, PieceLength, PeerID, InfoHash}, State) ->
    io:format("Spawning downloading FSM...\n"),
    gen_fsm:start(downloader_fsm, [Peers, Pieces, PieceLength, PeerID, InfoHash], []),
	{noreply, State};

handle_cast({notify_event, {parse_torrent_file,File}}, State) ->	
	spawn(open_file,start,[File]),
	{noreply, State};

handle_cast({notify_event, {torrent_file_content,Data}}, State) ->	
	spawn(torrent_file_parser,decode,[Data]),
	{noreply, State};

handle_cast({notify_event, {torrent_file_parsed_data,ParsedData}}, State) ->	
	spawn(file_records,toRec,[ParsedData]),
	{noreply, State};

handle_cast({notify_event, {torrent_record,Rec}}, State) ->	
	file_records:test(Rec),
	{noreply, State};

handle_cast(stop, State) -> {stop, normal, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.


%%==============================================================================
%% CONVENIENCE FUNCTIONS TO START AND STOP THE SERVER
%%==============================================================================

% @doc <p>Starts the server. See the module documentation for more details.</p>
% @spec start() -> {ok, Pid} | {error, Reason}
-spec start() -> {ok, Pid :: pid()} | {error, Reason :: term()}.
start() ->
	case gen_server:start({local, ?MODULE}, ?MODULE, [], []) of
		{ok, Pid} ->
			Pid;
		{error, Reason} ->
			error_logger:error_report({
				"Could not start the controller gen_server.",
				Reason}),
			{error, Reason}
	end.

% @doc <p>Stops the server. See the module documentation for more details.</p>
% @spec stop() -> ok | {error, stop_error}
-spec stop() -> ok | {error, stop_error}.
stop() ->
	case gen_server:cast(?MODULE, stop) of
		ok ->
			ok;
		_ ->
			{error, stop_error}
	end.

%%==============================================================================
%% PUBLIC FUNCTIONS 
%%==============================================================================

% @doc <p>Sends a gen_server message to start parsing the torrent file found at
% the path <code>File</code>. This function can be called from either the shell
% or the GUI.</p>
% @spec parse_torrent_file(File) -> ok
%     File = string()
-spec parse_torrent_file(File :: string()) -> ok.
parse_torrent_file(File) ->
	gen_server:call(?MODULE, {parse_torrent_file, File}).