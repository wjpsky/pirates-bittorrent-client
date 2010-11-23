%% Author: mahdi abdinejadi
%% Created: Nov 1, 2010
%% Description: this module connect to tracker and request for the peers list 
-module(tracker).


%%
%% Exported Functions
%%
-export([start/0,start/1]).

%% this function set an predefined request-which is used for 
%% the test purpose- then call start(predefined request). 
start()->
	PreDefinedRequest= "http://torrent.fedoraproject.org:6969/announce
    ?info_hash=%9F%A5%3C%C0%4B%16%38%F4%1C%7E%68%43%70%44%6B%34%9D%5F%62%37
    &peer_id=ABCDEFGHIJKLMNOPQRST&ip=193.11.196.59&port=6881&downloaded=10&
    left=1588&event=started",
	start(PreDefinedRequest).

%% this function call the inets:start() to have service for using
%% the http module function, then it send a request the url and get
%% get respond and parse it. 
start(Request)->
	inets:start(), 
	case httpc:request(Request) of
		{ok, {_,_,Respond}}-> simpleParser:decode(Respond);%io:format("test respond") ; 
		Error ->  error_logger:error_msg("An error occurred", [Error,?LINE,?MODULE]) %,io:format("test error")
	end.





