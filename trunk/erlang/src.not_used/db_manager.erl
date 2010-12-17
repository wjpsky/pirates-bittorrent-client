%% Author: nenastojova

%% Created: Nov 5, 2010
%% Description: TODO: Add description to db_manager
%%The db_manager module takes care of the two handler events the db_handler and the file_report handler
%%It serves for the purpose to receive an event and then passes it to the apropriate handler
-module(db_manager).
-export([startDb/0,startFile/1,stop/0,put/2,get/1]).

%creates an local manager and names it db it also adds the db_handler handler
startDb()->
    gen_event:start({local,db}),
	%db=manager name, db_handler = module name, []= argumets value passed to init in the db_handler
    gen_event:add_handler(db,db_handler,[]).

%adds the file_report handler to the db manager
startFile(File)->
    gen_event:add_handler(db,file_report,[File]).

%Handles the event of receiving a key and a value
put(Key,Value)->
    gen_event:notify(db,{put,Key,Value}).

%Handles a request of a value with specified key
get(Key)->
    gen_event:notify(db,{get,Key}).

%stops the db event manager
stop()->
    gen_event:stop(db).


