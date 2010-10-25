%%%-------------------------------------------------------------------
%%% File    : bencode.erl
%%% Author  : Mahdi Abdinejadi <mehdi@mehdi-laptop>
%%% Description : 
%%%
%%% Created : 15 Oct 2010 by mehdi <mehdi@mehdi-laptop>
%%%-------------------------------------------------------------------
-module(bencode).
-export([encode/1]).


                                                                   
encode(Data)->
    case is_integer(Data) of
	true->  en_integer(Data);
	false -> case is_list(Data) of
		     true-> en_string(Data);
		     false -> case is_binary(Data) of 
				  true -> en_binary(Data);
				  false -> case {list, List} = Data of
					       true-> en_list(List);	     
					       false ->case {dic, Dic} = Data of
							   true-> en_dic(Dic);
							   false -> {error, undefined_DataType}
						       end
					   end
			      end
		 end
    end.		      

en_integer(Int)->
   list_to_binary([$i]++integer_to_list(Int)++[$e]).

en_string(String)->    
   list_to_binary([size(list_to_binary(String))]++[$:]++String).

en_binary(Binary)->
   list_to_binary([size(Binary)]++[$:]++[Binary]). 

en_list(List)->
    ok.

en_dic(Dic)->
    ok.
