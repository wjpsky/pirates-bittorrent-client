%% Autor:Nena Stojova
%% Created: Nov 2, 2010
%% Description: This module just parses the .torrent file and stores the parsed
%% data in a list of tuples. the way the parsing is done is explained(commented) for every function individually.

%%==============================================================================
%% MODULE HEADER
%%==============================================================================
-module(torrent_file_parser).
-export([decode/1,fetchString/3]).


%Passes the data of the file to the rowdecode function that does the acctual decoding
decode(Data) ->
    {_Rest, [Result]} = rawdecode(Data, []),
    ParsedData=lists:reverse(Result),
	event_manager:notify({torrent_file_parsed_data,ParsedData}),
	ParsedData.

%%Used to store the information of a dictionary in a tuple. Since every dictionary has a name of a value and the information that that value holds we store this in a 
%%tuple format before appending it to the list, where the first value is the name of the value and the second the value itself.
to_tuple([]) ->
    [];
to_tuple([H]) ->
    [{unknown, H}];
to_tuple([H1, H2| T]) ->
    [{list_to_atom(H1), H2}| to_tuple(T)].

rawdecode([], Res) ->
    {[], Res};
%%When the first letter is $d we know we have encountered a dictionary so we convert it to a tuple add it to the result and continue rowdecoding with the rest of the information
rawdecode([$d| T], ["info"| Res]) ->
    {Rest, Dic} = rawdecode(T, []),
    InfoSize = length(T) - length(Rest),
    RawInfo = lists:sublist(T, InfoSize),
    Result = [to_tuple(Dic), "info"| Res],
    rawdecode(Rest, [[$d|RawInfo], "rawInfo"| Result]);

rawdecode([$d| T], Res) ->
    {Rest, Dic} = rawdecode(T, []),
    rawdecode(Rest, [to_tuple(Dic)| Res]);
%%Same as for dictionary but we just append the decoded list to the result without converting it to a tuple
rawdecode([$l| T], Res) ->
    {Rest, List} = rawdecode(T, []),
    rawdecode(Rest, [List| Res]);
%%When $e is encountered we know we have reached the end of a list or a dictionery so we just return what we have decoded untill now
rawdecode([$e| T], Res) ->
    {T, lists:reverse(Res)};
%%If the head is = $i, then we have a number to be decoded.
%%Since we know that after the extracted integer we have the letter e, we ignore it and pass the rest to the rawdecode to continue decoding
rawdecode([$i| T], Res) ->
    {Int, [_H| Rest]} = string:to_integer(T),%%Erlang bif,returns a tuple where the fisrt element is a number, and the rest after
    rawdecode(Rest, [Int| Res]);
%%If the head is larger than 47 or smaller than 58, then we know that we have a string to be decoded
%% Since we know that after the Size we have a column, we ignore the column and pass the rest 
%%to the fechString method,togather with the extracted Size
rawdecode([H| T], Res) when (H > 47) and (H < 58) ->
    {Size, [_Colon| Rest]} = string:to_integer([H| T]),%% Erlang bif ,returns a tuple where the first element is the size of the string and the second one is the string
    {Str, Others} = fetchString(Rest, Size, []),
    rawdecode(Others, [lists:reverse(Str)| Res]);
rawdecode([_H| T], Res) ->
    rawdecode(T, Res).

%%The method is receiving the Size of the String, the rest after it(variable S)and an empty list as u buffer.So it start extracting untill the Size=0 and appends it
%%to the buffer(the variable Str)
fetchString([],_Size,Str)->
    {Str,[]};
fetchString(S,0,Str) ->
    {Str,S};
fetchString([H|T],Size,Str) ->
    fetchString(T,Size-1,[H|Str]).

