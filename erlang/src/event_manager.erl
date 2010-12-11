%% Author: nenastojova
%% Created: Nov 27, 2010
%% Description: TODO: Add description to event_manager

-module(event_manager).
-behaviour (gen_server).
%%
%% Exported Functions
%%
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, start/0, stop/0,register/1,unregister/1,notify/1]).

%% =============================================================================
%% EXPORTED GEN_SERVER CALLBACKS
%% =============================================================================

init([]) -> {ok, []}.

handle_call(_Message, _From, State) -> {reply, ok, State}.

handle_cast({register,Pid}, State) ->
	{noreply,[Pid|State]};

handle_cast({unregister,Pid}, State) -> 
	{noreply,lists:delete(Pid, State)};
	
	
handle_cast({notify,Event}, State) ->
	[gen_server:cast(Pid,{notify_event,Event})||Pid<-State],
	{noreply,State};

handle_cast(stop, State) ->
	{stop, normal, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.


%%
%% EXPORTED CONVENIENCE FUNCTIONS TO START AND STOP THE SERVER
%%

start() ->
	case gen_server:start({local, ?MODULE}, ?MODULE, [], []) of
		{ok, Pid} ->
			{ok,Pid};
		{error, Reason} ->
			error_logger:error_report({"An error occurred", Reason, [{line,?LINE},{module,?MODULE}]})
	end.

stop() ->
	case gen_server:cast(?MODULE, stop) of
		ok ->
			ok;
		_ ->
			{error, stop_error}
	end.

%%
%% API Functions
%%
%The process who wants to be registered should be gen_server,
%then it will receive notifications by handle_cast
register(Pid)->
	gen_server:cast(?MODULE,{register,Pid}).

unregister(Pid)->
	gen_server:cast(?MODULE,{unregister,Pid}).

notify(Event)->
	gen_server:cast(?MODULE,{notify,Event}).
