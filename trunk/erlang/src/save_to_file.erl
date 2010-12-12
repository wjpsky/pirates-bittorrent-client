%% Author: Nena Stojova
%% Created: Dec 12, 2010
%% Description: This module receives a pieces of the file and stores them.
%% To run the code you just start the gen_server by calling the start method and pass the torrent record to the start.
%% Then you need to call save_piece for all the pieces that you have received
%%
%% By calling test a dummie torrent file is created which will be used to start saving.

-module (save_to_file).

-behaviour (gen_server).

%% EXPORT gen_server callback functions
-export ([code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2]).
%% EXPORT others
-export([start/1,create_file/2,save_piece/2]).
-export([xstart/0,xstop/0,test/0]).% testers

-include("torrent_records.hrl").
%%=============================================================================
%% API Functions
%%=============================================================================

start(Tor)->
	gen_server:start({local,?MODULE},?MODULE,Tor,[]).

% PData: Piece Data in Binary
save_piece(PNo,PData)->
	gen_server:cast(?MODULE,{save,PNo,PData}).


%%==============================================================================
%% Internal use functions
%%==============================================================================
open_files(Files)->
	open_files(Files,[]).
open_files([],OpenedFiles)-> lists:reverse(OpenedFiles);
open_files([File|T],OpenedFiles)->
	CreatedFile=create_file(File#torrent_file.path,File#torrent_file.length),
	open_files(T,[CreatedFile|OpenedFiles]).


create_file(Name,Size)->
	{ok,File}=file:open(Name,[write]),
	S=Size*8,
	file:write(File, << 0:S >> ),
	{File,Size}.




% PN: Piece Number
% PD: Piece Data
% PSize: Piece Size
% DSize is size of remaining Data of Piece


doSave(_PN,_PD,_DSize,_PSize,_Blocks,_Bytes,[])-> ok;
doSave(PN,PD,DSize,PSize,Blocks,Bytes,[{File,FSize}|T])->
	Pos=PSize*(PN-Blocks-1)-Bytes+(PSize-DSize),
	x({doSave,pn,PN,pd,PD,dsize,DSize,psize,PSize,blocks,Blocks,bytes,Bytes,{File,FSize},pos,Pos}),
	if
		( (Pos > -1) and (Pos < FSize) )->
			{NewPD,NewDSize}=savePieceHere(PN,PD,DSize,PSize,Blocks,Bytes,{File,FSize}),
			if (NewDSize > 0) ->
				{NewBlocks,NewBytes}=skipToNext(Blocks,Bytes,PSize,FSize),
				doSave(PN,NewPD,NewDSize,PSize,NewBlocks,NewBytes,T);
			true->
				ok
			end;
		true->
			{NewBlocks,NewBytes}=skipToNext(Blocks,Bytes,PSize,FSize),
			doSave(PN,PD,DSize,PSize,NewBlocks,NewBytes,T)
	end.

savePieceHere(PN,PD,DSize,PSize,Blocks,Bytes,{File,FSize})->
	Pos=PSize*(PN-Blocks-1)-Bytes+(PSize-DSize),
	x({savePieceHere,pn,PN,pd,PD,dsize,DSize,psize,PSize,blocks,Blocks,bytes,Bytes,{File,FSize},pos,Pos}),
	if
		((Pos+DSize) > FSize) ->
			DSize2=Pos+DSize-FSize,
			DSize1=DSize-DSize2,
			DS1=DSize1*8, DS2=DSize2*8,
			<< PD1:DS1,PD2:DS2 >> = PD,
			x({pos,Pos,dsize,DSize,fsize,FSize,dsize1,DSize1,dsize2,DSize2,ds1,DS1,ds2,DS2,pd,PD,pd1,binary_to_list(<<PD1:DS1>>),pd2,binary_to_list(<<PD2:DS2>>)});
		true->
			x("----------------------"),
			DS1=DSize*8,DS2=8,<<PD1:DS1>>=PD,<<PD2>>= <<0>>,DSize2=0
	end,
	file:pwrite(File,Pos,binary_to_list(<<PD1:DS1>>)),
	{<<PD2:DS2>>,DSize2}.

skipToNext(Blocks,Bytes,PSize,FSize)->
	AllPassed=Blocks*PSize+Bytes+FSize,
	x({passing,AllPassed,bytes}),
	NewBlocks=AllPassed div PSize,
	NewBytes=AllPassed rem PSize,
	{NewBlocks,NewBytes}.

test()->
	xstart(),
	xstop().
xstart()->
	F1=#torrent_file{length=115,path="part1.txt"},
	F2=#torrent_file{length=110,path="part2.txt"},
	Files=[F1,F2],
	Info=#torrent_info{name="Erlang User Conference",pieces="no pieces there yet",piece_length=25,files=Files},
	Tor=#torrent{announce="www.announce",info_hash="1234567890",info=Info},
	% x(Tor#torrent.info#torrent_info.files),
	% Tor#torrent.info#torrent_info.files.
	% x(["Tor generated: ",Tor]),
	start(Tor).
xstop()->
	save_piece(5,list_to_binary("ound the globe.The confer")),
	save_piece(8,list_to_binary("year, proof that the Erla")),
	save_piece(3,list_to_binary(" so far, with over 300 de")),
	save_piece(4,list_to_binary("legates gathering from ar")),
	save_piece(2,list_to_binary("onference was the biggest")),
	save_piece(9,list_to_binary("ng community is growing. ")),
	save_piece(1,list_to_binary("This year's Erlang User C")),
	save_piece(7,list_to_binary("ver 70 delegates on last ")),
	save_piece(6,list_to_binary("ence saw an increase of o")),
	gen_server:cast(?MODULE,stop).

x(S)->
	io:format("~p~n",[S]).


%%==============================================================================
%% GEN_SERVER CALLBACKS
%%==============================================================================
init(Tor)->
	Files=open_files(Tor#torrent.info#torrent_info.files),
	{ok,{Files,Tor}}.

handle_cast({save,PNo,PData}, State) ->
	{Files,Tor}=State,
	PSize=Tor#torrent.info#torrent_info.piece_length,
	doSave(PNo,PData,PSize,PSize,0,0,Files),
	{noreply,State};
handle_cast(stop, State) ->
	{stop,normal,State};
handle_cast(_AE, State) ->
	{noreply,State}.

handle_call(_Request, _From, State)->
	{noreply,State}.

code_change(_OldV,State,_Extra)->
	{ok,State}.

handle_info(_Info, State)->
	{noreply,State}.

terminate(_Reason, _State)->
	ok.
