%% Author: Navid Ranjbar, Niel Madlani
%% Created: Nov 2, 2010
%% Description: TODO: Add description to tcp_sup
-module(model_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_FileName) -> 
	UserChild = {model, {model, start_link, []}, permanent, 2000, worker, [model]},
	{ok, {{one_for_all, 1, 1}, [UserChild]}}.

