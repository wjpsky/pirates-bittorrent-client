%% Author: Mahdi Abdinejadi
%% Created: Nov 23, 2010
%% Description: TODO: This module simply generate an 20 char Peer Id
-module(peer_id).

-define(ID_LENGTH, 12).
-define(CHAR_LIST, "0123456789abcdefghijklmnopqrstuvwxyz"). %36 chars



%%----------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------
-export([get_id/0]).

%%----------------------------------------------------------------------
%% API Functions
%%----------------------------------------------------------------------
get_id()->
	random:seed(now()),
	"-PB0000-" ++ get_id(?ID_LENGTH , []).
 
%%----------------------------------------------------------------------
%% Local Functions
%%----------------------------------------------------------------------
get_id(0, Acc)->
	Acc;
get_id(Num, Acc)->
	Elem = lists:nth(round(random:uniform()*35)+1, ?CHAR_LIST),
	get_id(Num-1, [Elem | Acc]).
