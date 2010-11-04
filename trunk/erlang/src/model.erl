%% Authors: Jon Kristensen
%%
%% Description: This class knows about the current state of the application. The
%%              GUI queries this class through some kind of Data Access object
%%              to know what to render.
%%
%%              It implements the gen_server behaviour and thus defines a number
%%              of callbacks used by the gen_server module. See
%%              http://www.erlang.org/doc/man/gen_server.html for more
%%              information.
%%
%%              We need to start this process from a erl node started with "erl
%%              -sname 'erlangpbtc@localhost'".

-module(model).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, start/0, stop/0, get_state/0]).
-export([start_link/0]).
-record(state,{}).
%% =============================================================================
%% EXPORTED GEN_SERVER CALLBACKS
%% =============================================================================

init([]) -> {ok, #state{}}.

handle_call(get_state, _From, State) -> {reply, {}, State}.

handle_cast(stop, State) -> {stop, normal, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.


%% =============================================================================
%% EXPORTED CONVENIENCE FUNCTIONS TO START AND STOP THE SERVER
%% =============================================================================

start() ->
	case gen_server:start({local, ?MODULE}, ?MODULE, [], []) of
		{ok, Pid} ->
			Pid;
		{error, Reason} ->
			{error, Reason},
			error_logger:error_report("An error occurred", Reason, [?LINE,?MODULE])
	end.

stop() ->
	case gen_server:cast(?MODULE, stop) of
		ok ->
			ok;
		_ ->
			{error, stop_error}
	end.


%% =============================================================================
%% PUBLIC API FUNCTIONS
%% =============================================================================

get_state() -> gen_server:call(?MODULE, get_state).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
%% =============================================================================
%% LOCAL FUNCTIONS
%% =============================================================================