-module(download_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).
start_link() ->
    supervisor:start_link(download_sup, []).

init([]) -> 
	Child = {downloader_fsm, {tracker, start, []}, permanent, 2000, worker, [downloader_fsm]},
	{ok, {{one_for_all, 1, 1}, [Child]}}.
