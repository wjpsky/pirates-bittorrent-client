%%% -------------------------------------------------------------------
%%% Author  : Jon
%%% Description :
%%%
%%% Created : Nov 30, 2010
%%% -------------------------------------------------------------------
-module(downloader_fsm).

-behaviour(gen_fsm).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("torrent_records.hrl").

%% --------------------------------------------------------------------
%% External exports
-export([]).

%% gen_fsm callbacks
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

%% ====================================================================
%% External functions
%% ====================================================================


%% ====================================================================
%% Server functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, StateName, StateData}          |
%%          {ok, StateName, StateData, Timeout} |
%%          ignore                              |
%%          {stop, StopReason}
%% --------------------------------------------------------------------
init([Peers, Pieces, PieceLength, PeerID, InfoHash]) ->
	io:format("Generating peer FSMs...\n"),
	NewPieces = get_initial_pieces_data(Pieces),
	% Make the piece ID's zero-based.
	NewList = lists:map(
		fun(Elem) ->
			{Elem#piece.id - 1, Elem#piece{id = Elem#piece.id - 1}}
		end,
		NewPieces),
	NewDict = dict:from_list(NewList),
	generate_peer_fsms(Peers, PeerID, InfoHash),
    {ok, downloading, #state{pieces = NewDict, peers = Peers, peer_id = PeerID, info_hash = InfoHash, piece_size = PieceLength}}.

%% --------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
downloading({ready, BitField, Pid}, StateData) ->
	io:format("PeerFSM ready to download. Assigns piece, adding to dictionary...\n"),
	NewStateData = assign_piece(StateData, BitField, Pid),
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
downloading(Event, From, StateData) ->
	io:format("DOWNLOADING_FSM: downloading/3: Got ~p event!\n", [Event]),
    Reply = ok,
    {reply, Reply, state_name, StateData}.

%% --------------------------------------------------------------------
%% Func: handle_event/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
handle_event(Event, StateName, StateData) ->
	io:format("DOWNLOADING_FSM: handle_event/3: Got ~p event!\n", [Event]),
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
handle_sync_event(Event, From, StateName, StateData) ->
	io:format("sync\n", [Event]),
    Reply = ok,
    {reply, Reply, StateName, StateData}.

%% --------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
handle_info(Info, StateName, StateData) ->
	io:format("info\n", [Info]),
    {next_state, StateName, StateData}.

%% --------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%% --------------------------------------------------------------------
terminate(Reason, StateName, StatData) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/4
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState, NewStateData}
%% --------------------------------------------------------------------
code_change(OldVsn, StateName, StateData, Extra) ->
    {ok, StateName, StateData}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

generate_peer_fsms([], _, _) -> ok;
generate_peer_fsms([Peer|Tail], PeerID, InfoHash) ->
	{ok, Pid} = gen_fsm:start(peer_fsm, [Peer, self(), PeerID, InfoHash], []).
	% generate_peer_fsms(Tail, PeerID, InfoHash).

get_initial_pieces_data([]) -> [];
get_initial_pieces_data([{PieceID, PieceHash}|PieceTail]) ->
	[#piece{id = PieceID, hash = PieceHash}|get_initial_pieces_data(PieceTail)].

assign_piece(StateData, BitField, Pid) ->
	Hej = dict:to_list(dict:filter(
			fun(_Key, Value) ->
				Value#piece.status == inactive
			end,
			StateData#state.pieces)),
	PieceID = select_piece(Hej, BitField),
	case PieceID of
		-1 ->
			io:format("Decided on none...", [PieceID]),
			ok;
		_ ->
			io:format("Decided on ~p...", [PieceID]),
			gen_fsm:send_event(Pid, {download, PieceID}),
			ok
	end.

select_piece([], _) -> -1;
select_piece([{PieceID, _}|Tail], BitField) ->
	X = (PieceID - 1),
	
	% "If something is stupid, but works, then it's not stupid."
	SoUgly = 8 - (X rem 8),
	<<_:X/integer, Bit:1/integer, _:7/integer, _:SoUgly/integer, _/binary>> = BitField,
	
 	case Bit of
		0 ->
			io:format("-------------------------------------- ~p\n", [X]);
			% select_piece(Tail, BitField);
		1 ->
			PieceID
	end.
	