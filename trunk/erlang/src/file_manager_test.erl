%% Author: Navid Ranjbar
%% Created: Dec 15, 2010
%% Description: TODO: Add description to file_manager_test
-module(file_manager_test).

%%
%% Include files
%%
-include_lib("eunit/include/eunit.hrl").

%%
%% Test functions
%%


start_test() ->
	{H, _} = file_manager:start("something.iso", 100, 40, 1, "/"),
	?assertEqual(H, ok).

start2_test() ->
	exit(whereis(file_manager), kill),
	{H, _} = file_manager:start("something.iso", 100, 40, 1),
	?assertEqual(H, ok).

write_piece_data_test() ->
	?assertEqual(file_manager:write_piece_data("something.iso", 2, <<3,2>>)
																		, ok).
new_file_test() -> 
	?assertEqual(file_manager:new_file("navid.iso", 100, 20, 3), ok).