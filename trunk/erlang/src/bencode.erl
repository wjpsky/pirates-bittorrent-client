%%%-------------------------------------------------------------------
%%% File    : bencode.erl
%%% Author  : Mahdi Abdinejadi <mehdi@mehdi-laptop>
%%% Description : 
%%%
%%% Created : 15 Oct 2010 by mehdi <mehdi61b@gmail.com>
%%%-------------------------------------------------------------------
-module(bencode).
-export([encode/1,decode/1]).

%% this function take a binary and contert it to a list then encode 
encode(BinaryTerm)->
%% 	encod(binary_to_term(BinaryTerm)).
    encod(BinaryTerm).
	
encod({list,List})->
	en_list(List, [$l]);
encod({dic,Dic}) ->
	en_dic(Dic,[$d]);
encod(Data)->
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
	en_list(T, lists:reverse(encod(H))++AcList);
en_list([], AcList) ->
	lists:reverse(AcList)++[$e].

en_dic([[Key|Value]|T],AcDic)->
	en_dic(T, AcDic++encod(Key)++encod(Value));
%% en_dic([[[Key|Value1]|Value2]|[]], AcDic)->
%% 	en_dic([], AcDic++encod(Key)++encod(Value1)++encod(Value2));
en_dic([], AcList) ->
	AcList++[$e].

%% this function decode the bencode and convert it to respective 
%% term (dictionary, list, string and integer) 


decode(Data)->
%% 	term_to_binary([decode(Data, [])]).%% return the result as binary
  decode(Data,[]).

decode([], Ac) ->
	 Ac;
decode([$i|T], Ac)->
	   de_int(T, []);     	
decode([$l|T], Ac) ->
     de_list(T, []);
decode([$d|T], Ac) ->
 	  de_dic(T, []);
decode(Data, Ac) ->
    de_str(Data, []).


de_int([$e|Rest], AcInt)->
	{list_to_integer(AcInt), Rest};  %% return a tuple contain an integer and the rest of the list  
de_int([Elem|Rest], AcInt)->
	de_int(Rest, AcInt++[Elem]).
	
de_str([$:|Rest], AcStr)->
	lists:split(list_to_integer(AcStr), Rest);  %% reutrn a tuple contains a string and the rest of the list 
de_str([Elem|Rest], AcStr) ->
	de_str(Rest, AcStr++[Elem]).

de_list([$e|Rest], AcList)->
	{{list, AcList}, Rest};
de_list([Elem|Rest], AcList) ->
	{NewElems, NewRest} = decode([Elem|Rest], []),
    de_list(NewRest, AcList++[NewElems]).

de_dic([$e|Rest], AcList)->
	{{dic, AcList}, Rest};
de_dic([Elem|Rest], AcList) ->
	{NewElems, NewRest} = decode([Elem|Rest], []),
    de_dic(NewRest, AcList++[NewElems]).


    

	  
	  



