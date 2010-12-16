%% Author: Jianping Wang, Niel Madlani
%% Created: Dec 15, 2010
%% Description: A supervisor for the controller module. It will restart the controller module
%%				if it crashes.

-module(controller_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) -> 
	Child = {controller, {controller, start_link, []}, permanent, 2000, worker, [controller]},
	{ok, {{one_for_one, 1, 1}, [Child]}}.

