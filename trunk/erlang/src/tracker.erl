%%---------------------------------------------------------- 
%% @author mahdi abdinejadi <mehdi61b@gmail.com> @end
%% @version 0.1
%% @since Nov 1, 2010. 
%% @doc this module connect to tracker and request for the peers list. 
%% @end
%%----------------------------------------------------------
 
-module(tracker).

%% peer info
-record(peer,{
			  peer_id  :: string(),
			  ip       :: string(),
			  port     :: integer()
			  }).


%%
%% Exported Functions
%%
-export([start/0,start/1]).

%% this function set an predefined request-which is used for 
%% the test purpose- then call start(predefined request). 
start()->
	PreDefinedRequest= "http://torrent.fedoraproject.org:6969/announce?info_hash=%9F%A5%3C%C0%4B%16%38%F4%1C%7E%68%43%70%44%6B%34%9D%5F%62%37&peer_id=ABCDEFGHIJKLMNOPQRST&ip=193.11.196.59&port=6881&downloaded=10&left=1588&event=started",
	start(PreDefinedRequest).

%% @doc this function call the inets:start() to have service for using
%% the http module function, then it send a request the url and get
%% get respond and parse it. @end
-spec(start/1 :: (string()) -> term()).

start(Request)->
	inets:start(), 
	case httpc:request(Request) of
		{ok, {_,_,Respond}}-> Test = simpleParser:decode(Respond),%io:format("test respond") ;
								[{peers,List_peers},{interval,_Interval},{incomplete,_Incomplete},{complete,_Complete}] = Test,
%% 							  List_peers;
							  to_record(List_peers, []);
		Error ->  error_logger:error_msg("An error occurred", [Error,?LINE,?MODULE]) %,io:format("test error")
	end.

%% @doc this function convert a peer list to a list of peer record list.
-spec(to_record/2 :: ( term(),term()) -> term()).

to_record([], Acc)->
	Acc;
to_record([[{ip,Ip},{_peer_id,Peer_id},{port, Port}]| Rest], Acc)->
	Peer = #peer{ ip= Ip, peer_id = Peer_id, port= Port },
	to_record(Rest, [Peer|Acc]).

