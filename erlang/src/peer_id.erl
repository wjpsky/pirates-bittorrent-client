%%---------------------------------------------------------- 
%% @author Mahdi Abdinejadi <mehdi61b@gmail.com> @end
%% @version 0.1
%% @since Nov 23, 2010. 
%% @doc This module simply generate an 20 char Peer Id. 
%% @end
%%----------------------------------------------------------

-module(peer_id).

-define(ID_LENGTH, 12).
-define(CHAR_LIST, "0123456789abcdefghijklmnopqrstuvwxyz"). %36 chars

%%------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------
-export([get_id/0]).

%%------------------------------------------------------------------------------
%% API Functions
%%------------------------------------------------------------------------------

%% @doc Generate a random id with 20 char charector. @end
%% @spec (get_id()-> string())
-spec(get_id()-> string()).

get_id()->
	random:seed(now()),
	"-PB0000-" ++ get_id(?ID_LENGTH , []).
 
%%------------------------------------------------------------------------------
%% Local Functions
%%------------------------------------------------------------------------------

%% genrate an list of chars at random.
-spec(get_id/2 :: (integer(), list())-> list()).
get_id(0, Acc)->
	Acc;
get_id(Num, Acc)->
	Elem = lists:nth(round(random:uniform()*35)+1, ?CHAR_LIST),
	get_id(Num-1, [Elem | Acc]).
