%%==============================================================================
%% MODULE DESCRIPTION
%%==============================================================================

% @author Jon Kristensen

% @doc
% <p>This FSM module receives information from the tracker (including torrent,
% peer and piece information) and generates a FSM for every peer. The
% responsibility for this module should be to coordinate how the download is
% going, which peer is downloading what and to make sure to pass along finished
% pieces to the file_manager module. However, it's not fully implemented.</p>
% @end

%%==============================================================================
%% MODULE HEADER
%%==============================================================================

-module(downloader_fsm).

-behaviour(gen_fsm).

-include("torrent_records.hrl").

-export([init/1, downloading/2, downloading/3, handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-record(piece, {
	id :: integer(),
	hash :: list(), 
	status = inactive :: inactive | {downloading, pid()} | downloaded}).

-record(state, {
	peers :: [peer()],
	peer_fsms = dict:new() :: dict(),
	peer_id :: string(),
	info_hash :: binary(),
	pieces = dict:new() :: dict(),
	piece_size :: integer()}).

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
init([Peers, Pieces, PieceLength, PeerID, InfoHash]) ->
	io:format("Generating peer FSMs...\n"),
	
	% Create piece objects of the above piece record type.
	NewPieces = get_initial_pieces_data(Pieces),
	
	% Make the piece ID's zero-based and create a dictionary.
	ListOfPieces = lists:map(
		fun(Elem) ->
			{Elem#piece.id - 1, Elem#piece{id = Elem#piece.id - 1}}
		end,
		NewPieces),
	DictOfPieces = dict:from_list(ListOfPieces),

	% Start the peer FSM's.
	generate_peer_fsms(Peers, PeerID, InfoHash),
    {ok, downloading, #state{pieces = DictOfPieces, peers = Peers, peer_id = PeerID, info_hash = InfoHash, piece_size = PieceLength}}.

%% --------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
downloading({ready, BitField, Pid}, StateData) ->
	io:format("PeerFSM ready to download. Assigns piece, adding to dictionary...\n"),
	
	% TODO: The below function should update the state with information about
	% peer_fsm has been assigned the piece.
	assign_piece(StateData, BitField, Pid),

	{next_state, downloading, StateData#state{peer_fsms = dict:store(Pid, BitField, StateData#state.peer_fsms)}};
downloading(Event, StateData) ->
	io:format("DOWNLOADING_FSM: downloading/2: Got event ~p!\n", [Event]),
    {next_state, state_name, StateData}.

%% --------------------------------------------------------------------
%% Func: StateName/3
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}
%% --------------------------------------------------------------------
downloading(Event, _From, StateData) ->
	io:format("DOWNLOADING_FSM: downloading/3: Got event ~p!\n", [Event]),
    Reply = ok,
    {reply, Reply, state_name, StateData}.

%% --------------------------------------------------------------------
%% Func: handle_event/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
handle_event(Event, StateName, StateData) ->
	io:format("DOWNLOADING_FSM: handle_event/3: Got event ~p!\n", [Event]),
    {next_state, StateName, StateData}.

%% --------------------------------------------------------------------
%% Func: handle_sync_event/4
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}
%% --------------------------------------------------------------------
handle_sync_event(Event, _From, StateName, StateData) ->
	io:format("Got sync event: ~p\n", [Event]),
    Reply = ok,
    {reply, Reply, StateName, StateData}.

%% --------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
handle_info(Info, StateName, StateData) ->
	io:format("Got into message: ~p\n", [Info]),
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

%%==============================================================================
%% INTERNAL FUNCTIONS
%%==============================================================================

% @doc <p>Takes a list of peer information, our peer ID and the info hash and
% generates one peer_fsm. (Can be trivially modified to connect to all
% peers.)</p>
% TODO: Uncomment the last line for connecting to all peers.
% @spec generate_peer_fsms(Peers, PeerID, InfoHash) -> ok.
%     Peers = [peer()]
%     PeerID = string() 
%     InfoHash = binary() 
-spec generate_peer_fsms(Peers :: [peer()], PeerID :: string(),
	InfoHash :: binary()) -> ok.
generate_peer_fsms([], _, _) -> ok;
generate_peer_fsms([Peer|_Tail], PeerID, InfoHash) ->
	{ok, _} = gen_fsm:start(peer_fsm, [Peer, self(), PeerID, InfoHash], []).
	% generate_peer_fsms(Tail, PeerID, InfoHash).

% This function takes the piece data as delivered by the tracker module and
% converts it into a list of piece records.
-spec get_initial_pieces_data([{PieceID :: integer(),
    PieceHash :: [integer()]}]) -> [#piece{}].
get_initial_pieces_data([]) -> [];
get_initial_pieces_data([{PieceID, PieceHash}|PieceTail]) ->
	[#piece{id = PieceID, hash = PieceHash}|get_initial_pieces_data(PieceTail)].

% Takes the state data, a bit field (as sent in the peer_fsm BITFIELD message)
% and a peer_fsm Pid and assigns a piece number to that peer_fsm.
-spec assign_piece(#state{}, binary(), pid()) -> ok.
assign_piece(StateData, BitField, Pid) ->
	% Get a list of the dictionary tuples that represents inactive pieces.
	InactivePieces = dict:to_list(dict:filter(
			fun(_Key, Value) ->
				Value#piece.status == inactive
			end,
			StateData#state.pieces)),
	
	% Make a new list containing only the piece IDs.
	PieceIDs = lists:map(fun({PieceID, _}) -> PieceID end, InactivePieces),
	
	% Select one of the piece ID for the peer_fsm using select_piece/2 below.
	PieceID = select_piece(PieceIDs, BitField),
	
	% If PieceID is -1 then no piece was selected for the peer. Otherwise, we
	% ask the peer_fsm to download the chosen piece.
	case PieceID of
		-1 ->
			io:format("Decided on no piece for the peer...\n"),
			gen_fsm:send_event(Pid, disconnect), % TODO
			ok;
		_ ->
			io:format("Decided on piece ~p for the peer...\n", [PieceID]),
			gen_fsm:send_event(Pid, {download, PieceID}),
			ok
	end.

% Takes a list of piece IDs and a bit field and returns the ID of the selected
% piece.
-spec select_piece([integer()], binary()) -> integer().
select_piece([], _) -> -1;
select_piece([PieceID|Tail], BitField) ->
	% We calculate how many bits we have to read in order to be guaranteed to
	% read whole bytes when doing the bit field matching below. Remember: "If
	% something is stupid, but works, then it's not stupid."
	SoUgly = 8 - (PieceID rem 8),

	% Here we get the bit in the bit field that corresponds to the piece ID. If
	% it's a 1 then the peer has that particular piece.
	<<_:PieceID/integer, Bit:1/integer, _:7/integer, _:SoUgly/integer,
	  _/binary>> = BitField,
	
 	case Bit of
		0 ->
			% The peer does not have the piece. Let's try to select a new one.
			select_piece(Tail, BitField);
		1 ->
			% The peer have the piece. Return the piece ID.
			PieceID
	end.
	