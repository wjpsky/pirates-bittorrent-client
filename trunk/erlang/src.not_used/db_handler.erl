%% Author: nenastojova
%% Created: Nov 5, 2010
%% Description: TODO: Add description to db_handler
-module(db_handler).
-export([init/1,handle_event/2,terminate/2,code_change/3,handle_call/2,handle_info/2]).
-behaviour(gen_event).

% Db = State
init(Db)->
    {ok,Db}.

%Handles an event when key and value should be added to a list
%The state is the value of the Db that we pass when we start this event
handle_event({put,Key,Value},State)->
    {ok,[{Key,Value}|State]};

%Gets the value for that key
handle_event({get,Key},State) ->
    io:format("~w~n",[lists:keyfind(Key,1,State)]),
    {ok,State}.

terminate(_Args,_State)->
    ok.

code_change(_Old,State,_Extra)->
    {ok,State}.

handle_call(_Req,State)->
    {ok,ok,State}.

handle_info(_Info,State)->
    {ok,State}.
