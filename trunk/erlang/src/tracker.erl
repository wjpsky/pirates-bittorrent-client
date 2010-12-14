%%------------------------------------------------------------------------------
%% @author Mahdi Abdinejadi <mehdi61b@gmail.com> @end
%% @version 0.1
%% @since Nov 1, 2010. 
%% @doc This module connect to tracker and request for the peers list. 
%% @end
%%------------------------------------------------------------------------------
 
-module(tracker).

-include("torrent_records.hrl").


%%
%% Exported Functions
%%
-export([get_peers/2]).


%% @doc Calls the inets:start() to have service for using
%% the http module function, then it send a request the url and get
%% get respond and parse it to record. @end
%% @spec (get_peers/2 :: (term(), string()) -> ok|{error,term()})
-spec(get_peers/2 :: (term(), string()) -> ok|{error,term()}).

get_peers(File_record, Peer_id)->
	io:format("Contacting tracker..."), % for test and logging
	inets:start(),
	Request = generate_get_request(File_record, Peer_id),
	io:format("~p~n", [Request]),% for test and logging
	case httpc:request(Request) of
		{ok, {_,_,Respond}} ->
			io:format("Got tracker response..."),% for test and logging
			Parsed_respond = torrent_file_parser:decode(Respond),
			[{peers,List_peers},{interval,_Interval},{incomplete,_Incomplete},
			 	{complete,_Complete}] = Parsed_respond,
			Hello = to_peer_record_list(List_peers, []),
			io:format("Got ~p peers\n", [length(Hello)]),% for test and logging 
			gen_server:cast(controller, {got_peers, Hello, 
						File_record#torrent.info#torrent_info.pieces, 
						File_record#torrent.info#torrent_info.piece_length, 
						Peer_id, File_record#torrent.info_hash});
		Error ->  
			error_logger:error_msg("An error occurred", 
					[Error,?LINE,?MODULE]),% for test and logging
			Error
	end.


%% Convert a peer list to a list of peer record list.
-spec(to_peer_record_list/2 :: ( list(),list()) -> list()).

to_peer_record_list([], Acc)->
	Acc;
to_peer_record_list([[{ip,Ip},{'peer id',Peer_id},{port, Port}]| Rest], Acc)->
	Peer = #peer{ ip= Ip, peer_id = Peer_id, port= Port },
	to_peer_record_list(Rest, [Peer|Acc]).
 

%% Generate a string which will be send to tracker for get peer list. 
-spec(generate_get_request/2 :: (term(), string())-> string() ).

generate_get_request(Record, Peer_id)->
 	_Request = Record#torrent.announce ++ "?info_hash=" ++
		convert_byte_to_hex(
		binary_to_list(Record#torrent.info_hash), []) ++
		"&peer_id=" ++ Peer_id ++
		"&port=6881&downloaded=0&left=" ++
		integer_to_list(length(Record#torrent.info#torrent_info.pieces)) ++
		"&event=started".


%% Convert byte value from hash info to hex deciaml. 
-spec(convert_byte_to_hex/2 :: (list(),list())-> list() ).

convert_byte_to_hex([H | T] , Acc)-> 
	convert_byte_to_hex( T, Acc ++ "%" ++ io_lib:format("~2.16.0b", [H]));
convert_byte_to_hex([] , Acc) ->
	lists:flatten(Acc).


