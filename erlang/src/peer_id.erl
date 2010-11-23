%% Author: Mahdi Abdinejadi
%% Created: Nov 23, 2010
%% Description: TODO: This module simply generate an 20 char Peer Id
-module(peer_id).

-define(ID_LENGTH, 20).


%%----------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------
-export([get_id/0]).

%%----------------------------------------------------------------------
%% API Functions
%%----------------------------------------------------------------------
get_id()->
	get_id(20 , []).
 
%%----------------------------------------------------------------------
%% Local Functions
%%----------------------------------------------------------------------
get_id(0, Acc)->
	Acc;
get_id(Num, Acc)->
	Elem = lists:nth(round(random:uniform()*35)+1, get_char()),
%% 	io:format("~p \n",[Elem]),
	get_id(Num-1, [Elem | Acc]).

get_char()->
	"0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ".%length is 36 char
