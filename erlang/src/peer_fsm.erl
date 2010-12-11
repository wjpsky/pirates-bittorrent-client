%%==============================================================================
%% MODULE DESCRIPTION
%%==============================================================================

% @author Jon Kristensen

% @doc
% <p>One instance of this FSM module is spawned for every download and peer. Its
% state describes the session with the peer. It's not in any way complete, but
% it can connect to a peer, interpret commands, handshake, send and receive
% bitfields, upon a unchoke message request a piece and download that piece.</p>
% @end

%%==============================================================================
%% MODULE HEADER
%%==============================================================================

-module(peer_fsm).

-behaviour(gen_fsm).

-include("torrent_records.hrl").

-export([init/1, connecting/2, connecting/3, connected/2, connected/3, handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-define(REQUEST_LENGTH, 16384).

-record(state, {
	peer :: peer(),
	peer_id :: string(),
	info_hash :: binary(),
	downloader_pid :: pid(),
	socket = none :: none | gen_tcp:socket(),
	tcp_read_state = msg_length :: msg_length | msg_id | msg_payload,
	received_handshake = false :: true | false,
	msg_length_data = <<>> :: binary(),
	msg_id = undefined :: undefined | byte(),
	piece_id = undefined :: undefined | integer(),
	msg_buffer = <<>> :: binary(),
	msg_length = 0 :: integer(),
	data = <<>> :: binary(),
	self_choking = true :: true | false,
	self_interested = false :: true | false,
	peer_choking = true :: true | false,
	peer_interested = false :: true | false}).

%%==============================================================================
%% GEN_FSM CALLBACKS
%%==============================================================================

%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, StateName, StateData}          |
%%          {ok, StateName, StateData, Timeout} |
%%          ignore                              |
%%          {stop, StopReason}
%% --------------------------------------------------------------------
% Initializes this FSM. Takes the peer information, the downloader FSM pid (so
% that it can communicate back to the downloader process), the peer ID and the
% info hash, and sets the initial state.
init([Peer, DownloaderPid, PeerID, InfoHash]) ->
	io:format("In Peer FSM - connecting!\n"),
	State = #state{peer = Peer, downloader_pid = DownloaderPid,
		peer_id = PeerID, info_hash = InfoHash},
	gen_fsm:send_all_state_event(self(), connect),
    {ok, connecting, State}.

%% --------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
connecting(_Event, StateData) ->
	io:format("conn2 event"),
    {next_state, connecting, StateData}.
% We got a request to download a piece. Send the interest to the peer.
connected({download, PieceID}, StateData) ->
	io:format("========= Got request to download ~p... =========\n", [PieceID]),
	send_interest(StateData#state.socket),
    {next_state, connected, StateData#state{piece_id = PieceID}};
connected(disconnect, StateData) ->
	io:format("========= Got request to disconnect..."),
    {next_state, disconnected, StateData}.

%% --------------------------------------------------------------------
%% Func: StateName/3
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}
%% --------------------------------------------------------------------
connecting(_Event, _From, StateData) ->
	io:format("connecting event"),
    Reply = ok,
    {reply, Reply, connecting, StateData}.

connected(_Event, _From, StateData) ->
	io:format("connecting event"),
    Reply = ok,
    {reply, Reply, connecting, StateData}.

%% --------------------------------------------------------------------
%% Func: handle_event/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
% Connects to the peer. See the BitTorrent protocol version 1.0 for more
% information about the (handshake) data being sent.
handle_event(connect, _StateName, StateData) ->
	io:format("Connecting to ~p on port ~p...\n", [StateData#state.peer#peer.ip, StateData#state.peer#peer.port]),
	{ok, Socket} = gen_tcp:connect(StateData#state.peer#peer.ip, StateData#state.peer#peer.port, [binary, {active, true}]),
	ok = gen_tcp:send(
		Socket, [
			<<19, "BitTorrent protocol", 0, 0, 0, 0, 0, 0, 0, 0>>,
			StateData#state.info_hash,
			list_to_binary(StateData#state.peer_id)]),
    {next_state, connected, StateData#state{socket = Socket}};
handle_event(A, B, C) ->
	io:format("~p~p~p", [A, B, C]).

%% --------------------------------------------------------------------
%% Func: handle_sync_event/4
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}
%% --------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, StateData) ->
	io:format("Got a sync_event in peer_fsm!\n"),
    Reply = ok,
    {reply, Reply, StateName, StateData}.

%% --------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
%
% Network data received. We process it in process_tcp_data.
handle_info({tcp, _Port, Data}, connected, StateData) ->
	io:format("Received network data...\n", []),
	{NewStateName, NewStateData} = process_tcp_data(Data, connected, StateData),
    {next_state, NewStateName, NewStateData};

% Peer closed the connection. Change the state name to disconnected.
handle_info({tcp_closed, _Port}, _StateName, StateData) ->
	io:format("Peer closed connection.\n"),
    {next_state, disconnected, StateData};

% The connection to the peer timed out. Change the state name to disconnected.
handle_info({error,etimedout}, _StateName, StateData) ->
	io:format("Connection timed out.\n"),
    {next_state, disconnected, StateData};

% The connection to the peer was refused. Change the state name to disconnected.
handle_info({error,econnrefused}, _StateName, StateData) ->
	io:format("Connection refused error.\n"),
    {next_state, disconnected, StateData};

handle_info(Info, StateName, StateData) ->
	io:format("some other event ~p", [Info]),
    {next_state, StateName, StateData}.

%% --------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%% --------------------------------------------------------------------
terminate(_Reason, _StateName, _StatData) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/4
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState, NewStateData}
%% --------------------------------------------------------------------
code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

% process_tcp_data is a function that takes some data (as received by the tcp
% message above) and processes it, producing messages as well as updating the
% state of the FSM. A message contains a message length (four bytes), possibly a
% message ID (one byte) and if it has a message ID it also possibly has payload
% data (an arbitrary amount of bytes). This is all taken care of by this
% function so that functions such as process_msg don't have to care about it.

-spec process_tcp_data(binary(), connected, #state{}) ->
	{connected | disconnected, #state{}}.

% Basecase; no more data to process, return new state.
process_tcp_data(<<>>, StateName, StateData) ->
	io:format("Hit process_tcp_data basecase, returning new state.\n"),
	{StateName, StateData};

% We have not yet received the whole handshake message.
process_tcp_data(Data, StateName, StateData = #state{received_handshake = false}) ->
	io:format("Processing handshake data...\n"),
	BufferPlusData = buffer_plus_data(StateData, Data),
	<<Pstrlen, _/binary>> = BufferPlusData,
	case (byte_size(BufferPlusData) >= (Pstrlen + 49)) of
		true ->
			% The available data covers the handshake. Extract the handshake
			% data and process the rest.
			io:format("Got the handshake data I need...\n"),
			X = Pstrlen + 49,
			<<Handshake:X/binary, Rest/binary>> = BufferPlusData,
			process_handshake(Handshake, StateData),
			io:format("Handshake processed...\n"),
			process_tcp_data(Rest, StateName,
				StateData#state{received_handshake = true});
		false ->
			% We have not received enough data for a handshake. Save it in the
			% buffer and process it later.
			io:format("Not enough data for handshake...\n"),
			process_tcp_data(<<>>, StateName,
				StateData#state{msg_buffer = BufferPlusData})
	end;

% Matches if we are currently processing the message length. Extracts a message
% length byte and continues processing the tail.
process_tcp_data(
  <<Byte:1/binary, Rest/binary>>,
  StateName,
  StateData = #state{tcp_read_state = msg_length, msg_length_data = MsgLengthData}) ->
	io:format("Processing msg length data, got byte ~p...\n", [Byte]),
	NewMsgLengthData = <<MsgLengthData/binary, Byte/binary>>,
	case byte_size(NewMsgLengthData) of
		4 ->
			<<X:32>> = NewMsgLengthData,
			io:format("Got message length: ~p\n", [X]),
			% The message length length data has been fully read.
			case X of
				0 ->
					% This is a keep-alive message. Process it and continue
					% looking for message length.
					io:format("... It must be a keep-alive message!\n"),
					{NewStateName, NewStateData} = process_msg(0, none, none,
						StateName, StateData),
					process_tcp_data(Rest, NewStateName,
						NewStateData#state{msg_length_data = <<>>});
				_ ->
					% This is not a keep-alive message.
					io:format("It's not a keep-alive message...\n"),
					NewState = StateData#state{tcp_read_state = msg_id,
						msg_length_data = <<>>, msg_length = X},
					process_tcp_data(Rest, StateName, NewState)
			end;
		_ ->
			io:format("Not enough bytes for putting together message length...\n"),
			% We need to read additional bytes to get the message length.
			NewState = StateData#state{msg_length_data = NewMsgLengthData},
			process_tcp_data(Rest, StateName, NewState)
	end;

% Matches if we are currently processing the message ID. Will extract the byte
% we need and continue processing the tail for the message payload.
process_tcp_data(
  <<Byte:1/binary, Rest/binary>>,
  StateName,
  StateData = #state{tcp_read_state = msg_id, msg_length = MsgLength}) ->
	io:format("Processing msg id data... ~p\n", [Byte]),
	case MsgLength of
		1 ->
			% This is a message without a payload. We process it and continue
			% to look for message length.
			{NewStateName, NewStateData} = process_msg(1, Byte, none, StateName,
				StateData),
			process_tcp_data(Rest, NewStateName,
				NewStateData#state{tcp_read_state = msg_length});
		_ ->
			% This is a message with a payload. We need to read the payload.
			process_tcp_data(Rest, StateName,
				StateData#state{tcp_read_state = msg_payload, msg_id = Byte})
	end;

% Payload data received. If it together with the buffer is enough to produce a
% message we do so, otherwise we add it to the buffer and wait for more data.
process_tcp_data(Data, StateName, StateData = #state{tcp_read_state = msg_payload}) ->
	BufferPlusData = buffer_plus_data(StateData, Data),
	io:format("Comparing ~p and ~p...\n", [byte_size(BufferPlusData) + 1,
		StateData#state.msg_length]),
	% We see if the buffer data plus the size of the message ID is greater than
	% or equal to the message length.
	case byte_size(BufferPlusData) + 1 >= StateData#state.msg_length of
		true ->
			io:format("We have enough data to process the message.\n"),
			% We got enough data to process the message. Separate BufferPlusData
			% into Payload and Rest.
			PayloadLength = StateData#state.msg_length - 1,
			<<Payload:PayloadLength/binary, Rest/binary>> = BufferPlusData,
			{NewStateName, NewStateData} = process_msg(StateData#state.msg_length,
				StateData#state.msg_id, Payload, StateName, StateData),
			process_tcp_data(Rest, NewStateName,
				NewStateData#state{tcp_read_state = msg_length, msg_id = undefined,
				msg_buffer = <<>>, msg_length = 0});
		false ->
			io:format("We don't have enough data to process the message.\n"),
			% We did not get enough data to process the message. Put everything
			% in the buffer and continue.
			process_tcp_data(<<>>, StateName,
				StateData#state{msg_buffer = BufferPlusData})
	end.

% This function is without side-effects, except for the possibility of crashing
% the FSM when the handshake information from the peer is illegal.
-spec process_handshake(binary(), #state{}) -> ok.
process_handshake(
  <<_Pstrlen, _Pstr:_Pstrlen/binary, _Flags:8/binary, InfoHash:20/binary,
	_PeerID:20/binary>>,
  #state{info_hash = InfoHash}) ->
	ok.

% These functions all processes messages sent by the peer. Some of them sends
% commands to the peer in response, but most of them just print debug output.
process_msg(0, none, none, StateName, StateData) ->
	io:format(">>> Received a KEEP ALIVE message.\n"),
	{StateName, StateData};
process_msg(1, <<0>>, none, StateName, StateData) ->
	io:format(">>> Received a CHOKE message.\n"),
	{StateName, StateData};
process_msg(1, <<1>>, none, StateName, StateData) ->
	io:format(">>> Received an UNCHOKE message, sending REQUEST.\n"),
	send_request(StateData#state.socket, StateData#state.piece_id, trunc(bit_size(StateData#state.data) / ?REQUEST_LENGTH)),
	{StateName, StateData};
process_msg(1, <<2>>, none, StateName, StateData) ->
	io:format(">>> Received an INTERESTED message.\n"),
	{StateName, StateData};
process_msg(1, <<3>>, none, StateName, StateData) ->
	io:format(">>> Received a NOT INTERESTED message.\n"),
	{StateName, StateData};
process_msg(5, <<4>>, PieceIndex, StateName, StateData) ->
	io:format(">>> Received a HAVE message (~p).\n", [PieceIndex]),
	{StateName, StateData};
process_msg(Length, <<5>>, Bitfield, StateName, StateData) ->
	io:format(">>> Received a BITFIELD message.\n"),
	gen_fsm:send_event(StateData#state.downloader_pid, {ready, Bitfield, self()}),
	send_bitfield(StateData#state.socket, Length - 1),
	{StateName, StateData};
process_msg(259, <<6>>, IndexBeginLength, StateName, StateData) ->
	io:format(">>> Received a REQUEST message (~p).\n", [IndexBeginLength]),
	{StateName, StateData};
process_msg(_Length, <<7>>,
  <<_Index:32/integer, _Begin:32/integer, _Block/binary>>, StateName, StateData) ->
	io:format(">>> Received a PIECE message. Storing...\n"),
	% TODO: We might want to send an additional request here.
	% send_request(StateName#state.socket, Index, Begin + ?REQUEST_LENGTH),
	{StateName, StateData};
process_msg(13, <<8>>, IndexBeginLength, StateName, StateData) ->
	io:format(">>> Received a CANCEL message (~p).\n", [IndexBeginLength]),
	{StateName, StateData};
process_msg(3, <<9>>, ListenPort, StateName, StateData) ->
	io:format(">>> Received a PORT message (~p).\n", [ListenPort]),
	{StateName, StateData};
process_msg(Length, ID, Payload, StateName, StateData) ->
	io:format(">>> Uncaught message: ~p, ~p, ~p\n", [Length, ID, Payload]),
	{StateName, StateData}.

% Simply joins the buffer and the data, making the above code more readable.
-spec buffer_plus_data(#state{}, binary()) -> binary().
buffer_plus_data(#state{msg_buffer = MsgBuffer}, Data) ->
	<<MsgBuffer/binary, Data/binary>>.

% Send an INTEREST message to the peer.
-spec send_interest(tcp:socket()) -> ok.
send_interest(Socket) ->
	io:format(">>>>>>>>>>>>> SENDING INTEREST <<<<<<<<<<<<<\n"),
	ok = gen_tcp:send(Socket, [
		<<0,0,0,1>>, % Message length
		<<2>>]). % Message ID

% Send a REQUEST message to the peer. Begin is the byte where we want to start
% downloading.
-spec send_request(tcp:socket(), integer(), binary()) -> ok.
send_request(Socket, PieceID, Begin) ->
	io:format(">>>>>>>>>>>>> SENDING REQUEST <<<<<<<<<<<<<\n"),
	ok = gen_tcp:send(Socket, [
		<<0,0,1,3>>, % Message length
		<<6>>, % Message ID
		<<PieceID:32/integer>>, % Index
		<<Begin:32/integer>>, % Begin
		<<?REQUEST_LENGTH:32/integer>>]). % Requested length

% Send a BITFIELD message to the peer.
% TODO: This message should send the actual state, the bitfield generated below
% is only correct while no pieces have been downloaded.
-spec send_bitfield(tcp:socket(), integer()) -> ok.
send_bitfield(Socket, Length) ->
	io:format(">>>>>>>>>>>>> SENDING BITFIELD <<<<<<<<<<<<<\n"),
	MsgLength = 1 + Length,
	ok = gen_tcp:send(Socket, [
		<<MsgLength:32/integer>>, % Message length
		<<5>>, % Message ID
		binary:copy(<<0>>, Length)]). % Requested length
