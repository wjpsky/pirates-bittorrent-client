%% Author: Navid Ranjbar
%% Created: Dec 17, 2010
%% Description: TODO: Add description to tracker_test
-module(tracker_test).

%%
%% Include files
%%

-include_lib("eunit/include/eunit.hrl").
-include("torrent_records.hrl").

%%
%% API Functions
%%

get_peers_test() ->
	A = file_records:toRec(open_file:start("1.torrent")),
	?assertEqual(ok, tracker:get_peers(A, peer_id:get_id())).
	
get_peers2_test() ->
	?assertError({badrecord,torrent}, tracker:get_peers([], peer_id:get_id())).