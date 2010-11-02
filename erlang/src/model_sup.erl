%% Author: Navid Ranjbar, Niel Madlani
%% Created: Nov 2, 2010
%% Description: TODO: Add description to tcp_sup
-module(model_sup).
-export([start/0, init/1]).

start() -> supervisor:start_link(?MODULE, []).

init(_FileName) -> 
	UserChild = {model, {model, start, []}, permanent, 2000, worker, [model]},
	{ok, {{one_for_all, 1, 1}, [UserChild]}}.

