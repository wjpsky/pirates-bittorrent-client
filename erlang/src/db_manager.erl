%% Author: nenastojova
%% Created: Nov 5, 2010
%% Description: TODO: Add description to db_manager
-module(db_manager).
-export([startDb/0,startFile/1,stop/0,put/2,get/1]).

startDb()->
    gen_event:start({local,db}),
    gen_event:add_handler(db,db_handler,[]).

startFile(File)->
    gen_event:add_handler(db,file_report,[File]).
    
put(Key,Value)->
    gen_event:notify(db,{put,Key,Value}).

get(Key)->
    gen_event:notify(db,{get,Key}).

stop()->
    gen_event:stop(db).


