%% Author: nenastojova
%% Created: Nov 2, 2010
%% Description: TODO: Add description to fileRecords
-module(fileRecords).
-export([toRec/1
%% 		 ,test/1
		]).
-include("torrent_records.hrl"). 

toRec(Data)->
    toRec(Data,#torrent{}).

toRec([],Tor)->
    Tor;
toRec([{announce,Val}|T],Tor) ->
    toRec(T,Tor#torrent{announce=Val});
toRec([{info,Val}|T],Tor) ->           
    toRec(T,Tor#torrent{info=toInfoRec(Val,#torrent_info{})});
toRec([{rawInfo,Val}|T],Tor)->
    Hash=hash_info(Val),
    toRec(T,Tor#torrent{info_hash=Hash});
toRec([_H|T],Tor) -> 
    toRec(T,Tor).

toInfoRec([],Info)->
    Info;
toInfoRec([{files,Val}|T],Info)->
     toInfoRec(T,Info#torrent_info{files=fetchFiles(Val,[])});
toInfoRec([{name,Val}|T],Info) ->
    toInfoRec(T,Info#torrent_info{name=Val});
toInfoRec([{'piece length',Val}|T],Info) ->
     toInfoRec(T,Info#torrent_info{piece_length=Val});
toInfoRec([{pieces,Val}|T],Info) ->
    toInfoRec(T,Info#torrent_info{pieces=fetchPieces(Val,1)}).

fetchPieces([],_Key)->
    [];
fetchPieces(List,Key) ->
    {Hash,Rest}=simpleParser:fetchString(List,20,[]),
    [{Key,Hash}|fetchPieces(Rest,Key+1)].

fetchFiles([],Result)->
    Result;
fetchFiles([H|T],Result) ->
    File=decodeFile(H,#torrent_file{}),
    fetchFiles(T,[File|Result]).

decodeFile([],Tor)->
    Tor;
decodeFile([{length,Len}|T],Tor) ->
    decodeFile(T,Tor#torrent_file{length=Len});
decodeFile([{path,Path}|T],Tor) ->
    decodeFile(T,Tor#torrent_file{path=convertPath(Path)}).

convertPath([])->
    [];
convertPath([H|[]])->
    H;
convertPath([H|T]) ->
    H++"/"++convertPath(T).

hash_info(List)->
    crypto:start(),
    Hash=crypto:sha(List),
    crypto:stop(),
    Hash.

%%test(Rec)->
  %%  (Rec#torrent.info)#torrent_info.name,
   %% Rec#torrent.info_hash.
