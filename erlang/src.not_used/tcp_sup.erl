%% Author: Navid Ranjbar, Niel Madlani
%% Created: Nov 2, 2010
%% Description: TODO: Add description to tcp_sup
-module(tcp_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) -> 
	UserChild = {tcp, {tcp, start_link, []}, permanent, 2000, worker, [tcp]},
	{ok, {{one_for_all, 1, 1}, [UserChild]}}.

