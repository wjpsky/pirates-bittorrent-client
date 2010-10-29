-module(simpleParser).
-export([start/1]).
-include_lib("kernel/include/file.hrl").

start(File)->
    case (file:open(File,[read])) of
	{ok,IoDevice}->
	    {ok,FileInfo} = file:read_file_info(File),
	    case (file:read(IoDevice,FileInfo#file_info.size)) of
		{ok,Data} ->
		    decode(Data);
		A ->
		    A
	    end;
	{error,Reason}->
	    {File,error,Reason}
    end.
decode(Data)->
    {_Rest,[Result|[]]}=rawdecode(Data,[]),
    lists:reverse(Result).

to_tuple([])->
    [];
to_tuple([H|[]])->
    [{unknown,H}];
to_tuple([H1,H2|T])->
    [{list_to_atom(H1),H2}|to_tuple(T)].

rawdecode([],Res)->
    {[],Res};
rawdecode([$d|T],Res)->
    {Rest,Dic}=rawdecode(T,[]),
    rawdecode(Rest,[to_tuple(Dic)|Res]);
rawdecode([$l|T],Res)->
    {Rest,List}=rawdecode(T,[]),
    rawdecode(Rest,[List|Res]);
rawdecode([$e|T],Res)->
    {T,lists:reverse(Res)};
rawdecode([$i|T],Res)->
    {Int,[_H|Rest]} = string:to_integer(T),
    rawdecode(Rest,[Int|Res]);
rawdecode([H|T],Res) when ((H > 47) and (H < 58))->
    {Size,[_Colon|Rest]} = string:to_integer([H|T]),
    {Str,Others} = fetchString(Rest,Size,[]),
    rawdecode(Others,[lists:reverse(Str)|Res]);
rawdecode([_H|T],Res) ->
    rawdecode(T,Res).


fetchString([],_Size,Str)->
    {Str,[]};
fetchString(S,0,Str) ->
    {Str,S};
fetchString([H|T],Size,Str) ->
    fetchString(T,Size-1,[H|Str]).

