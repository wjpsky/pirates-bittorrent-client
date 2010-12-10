%%---------------------------------------------------------- 
%% @author mahdi abdinejadi <mehdi61b@gmail.com> @end
%% @version 0.1
%% @since Nov 1, 2010. 
%% @doc this module connect to tracker and request for the peers list. 
%% @end
%%----------------------------------------------------------
 
-module(tracker).

-include("torrent_records.hrl").


%%
%% Exported Functions
%%
-export([start/0,start/2,convert_byte_to_hex/2, get_hash_info/1]).

%% this function set an predefined request-which is used for 
%% the test purpose- then call start(predefined request). 
start()->
	PreDefinedRequest= "http://torrent.fedoraproject.org:6969/announce?info_hash=%9F%A5%3C%C0%4B%16%38%F4%1C%7E%68%43%70%44%6B%34%9D%5F%62%37&peer_id=ABCDEFGHIJKLMNOPQRST&port=6881&downloaded=10&left=1588&event=started".
	                 % http://torrent.fedoraproject.org:6969/announce?info_hash=%257d%25ef%257d%2504%2577%25b9%25cb%25d1%2543%25e3%25ba%25ca%25e7%252e%253f%2525%255f%253c%257e%2546&peer_id=ABCDEFGHIJKLMNOPQRST&port=6881&downloaded=10&left=1588&event=started
	
%% 	start(PreDefinedRequest).

%% @doc this function call the inets:start() to have service for using
%% the http module function, then it send a request the url and get
%% get respond and parse it. @end
%% -spec(start/1 :: (string()) -> term()).

start(Record, Peer_id)->
	io:format("Contacting tracker..."),
	inets:start(),
	Request = generate_get_request(Record, Peer_id),
	io:format("~p~n", [Request]),
	case httpc:request(Request) of
		{ok, {_,_,Respond}} ->
			io:format("Got tracker response..."),
			Test = torrent_file_parser:decode(Respond),
			io:format("~p ", [Test]),
			[{peers,List_peers},{interval,_Interval},{incomplete,_Incomplete},{complete,_Complete}] = Test,
			Hello = to_peer_record_list(List_peers, []),
			io:format("Got ~p peers...\n", [length(Hello)]),
			gen_server:cast(controller, {got_peers, Hello, Record#torrent.info#torrent_info.pieces, Record#torrent.info#torrent_info.piece_length, Peer_id, Record#torrent.info_hash});
		Error ->  error_logger:error_msg("An error occurred", [Error,?LINE,?MODULE]) %,io:format("test error")
	end.

%% @doc this function convert a peer list to a list of peer record list.
-spec(to_peer_record_list/2 :: ( term(),term()) -> term()).

to_peer_record_list([], Acc)->
	Acc;
to_peer_record_list([[{ip,Ip},{'peer id',Peer_id},{port, Port}]| Rest], Acc)->
	Peer = #peer{ ip= Ip, peer_id = Peer_id, port= Port },
	to_peer_record_list(Rest, [Peer|Acc]).
 
generate_get_request(Record, Peer_id)->
 	Request = Record#torrent.announce ++ "?info_hash=" ++
		convert_byte_to_hex(
		binary_to_list(Record#torrent.info_hash), []) ++
		"&peer_id=" ++ Peer_id ++
		"&port=6881&downloaded=0&left=" ++
		integer_to_list(length(Record#torrent.info#torrent_info.pieces)) ++
		"&event=started".

convert_byte_to_hex([H | T] , Acc)-> 
	convert_byte_to_hex( T, Acc ++ "%" ++ io_lib:format("~2.16.0b", [H]));
convert_byte_to_hex([] , Acc) ->
	lists:flatten(Acc).

get_hash_info(Data)->
	[_,  {_, Info}, _ , _]  =  Data,
	io:format("Infoooooo ~p endddddd", [Info]).