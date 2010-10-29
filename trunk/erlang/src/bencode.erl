%%%-------------------------------------------------------------------
%%% File    : bencode.erl
%%% Author  : Mahdi Abdinejadi <mehdi@mehdi-laptop>
%%% Description : 
%%%
%%% Created : 15 Oct 2010 by mehdi <mehdi61b@gmail.com>
%%%-------------------------------------------------------------------
-module(bencode).
-export([encode/1,decode/2]).




encode({list, List})->
	en_list(List, [$l]);
encode({dic, Dic}) ->
	en_dic(Dic,[$d]);
encode(Data)->
	case is_integer(Data) of
		true->  en_integer(Data);
		false -> case is_list(Data) of
					 true-> en_string(Data);
					 false -> {error, undefined_DataType}
				 end
	end.		      

en_integer(Int)->
	[$i]++integer_to_list(Int)++[$e].

en_string(String)->    
	integer_to_list(length(String))++[$:]++String.

en_list([H|T], AcList)->
	en_list(T, lists:reverse(encode(H))++AcList);
en_list([], AcList) ->
	lists:reverse(AcList)++[$e].

en_dic([{Key,Value}|T],AcDic)->
	en_dic(T, AcDic++encode(Key)++encode(Value));
en_dic([], AcList) ->
	AcList++[$e].


decode([], Ac) ->
	{[], Ac};
decode([$e|T],Ac)->
	[$e|T];
decode([$i|T], Ac)->
	{Rest, Int} = de_int(T, []),
	{Rest, Ac++[Int]};     
decode([$l|T], Ac) ->
	{Rest, List} = de_list(T, []),
	decode(Rest,Ac++[{list,List}]);
decode([$d|T], Ac) ->
	{Rest, Dic} = de_list(T, []),
	decode(Rest, Ac++[{dic,Dic}]);
decode(Data, Ac) ->
	{Str, Rest} = de_str(Data, []),
	{Rest, Str}.
%    decode(Rest, Ac++[Str]).

de_str([$:|Rest], Ac)->
	%    io:format("##~p##",[Rest]),
	lists:split(list_to_integer(Ac), Rest);
de_str([Elem|Rest], Ac) ->
	de_str(Rest, Ac++[Elem]).

de_list([$e|Rest], Ac)->
	{Rest, Ac};
de_list(List, Ac) ->
	{Rest, Result} = decode(List, []),
	de_list(Rest, Ac++Result).

de_int([$e|Rest], AcInt)->
	{Rest,list_to_integer(AcInt)};
de_int([Elem|Rest], AcInt)->
	de_int(Rest, AcInt++[Elem]).
