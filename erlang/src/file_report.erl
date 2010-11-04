%% Author: nenastojova
%% Created: Nov 5, 2010
%% Description: TODO: Add description to file_report
-module(file_report).

-export([init/1,handle_event/2,terminate/2,code_change/3,handle_call/2,handle_info/2]).
-behaviour(gen_event).

init(File)->
    {ok,Fd}=file:open(File,write),
    {ok,Fd}.

handle_event(Event,Fd)->
    io:format(Fd,"~w~n",[{erlang:localtime(),Event}]),
    {ok,Fd}.

terminate(_Args,_State)->
    ok.

code_change(_Old,State,_Extra)->
    {ok,State}.

handle_call(_Req,State)->
    {ok,ok,State}.

handle_info(_Info,State)->
    {ok,State}.
