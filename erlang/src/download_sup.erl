-module(download_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) -> 
	Child = {downloader_fsm, {downloader_fsm, start_link, []}, permanent, 2000, worker, [downloader_fsm]},
	{ok, {{one_for_all, 1, 1}, [Child]}}.
