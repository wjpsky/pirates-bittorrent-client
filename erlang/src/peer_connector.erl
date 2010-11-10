%% Author: nenastojova
-module(peer_connector).
-export([code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,start_link/7,stop/2,stop/1]).

-behaviour(gen_server).

code_change(_OldVersion,State,_Extra)->
    {ok,State}.

handle_call(_Request,_From,State)->
    {noreply,State}.

handle_cast({stop,Reason},_State)->
    {stop,Reason};
handle_cast({packet_received,Packet},{Socket,Info_hash,Piece_number,Piece_length,Piece_hash,Received_length,ThePiece,My_id})->
    New_rec_len=Received_length+length(Packet),
    New_piece=ThePiece++Packet,
    NewState={Socket,Info_hash,Piece_number,Piece_length,Piece_hash,New_rec_len,New_piece,My_id},
    if
	(New_rec_len==Piece_length)->
	    check_hash(NewState);
	true->
	    receive_packet(Socket,My_id),
	    {noreply,NewState}
    end;
handle_cast(_Request,State)->
    {noreply,State}.

handle_info(_Info,State)->
    {noreply,State}.

init({Ip,Port,Info_hash,Piece_number,Piece_length,Piece_hash,My_id})->
    %% establish connection
    %% connect and get socket
    Socket=connect(Ip,Port),
    %% send handshake and get respond
    %% depends on result, stop or send piece request
    case handshake(Socket,Info_hash,My_id) of
	ok->
	    case send_request(Socket,Piece_number) of
		ok->
		    receive_packet(Socket,My_id);
		{error,Reason}->
		    stop(My_id,Reason)
	    end;
	{error,Reason} ->
	    stop(My_id,Reason)
    end,
    %% finally in loop it will receive  bytes of piece
    {ok,{Socket,Info_hash,Piece_number,Piece_length,Piece_hash,0,[],My_id}}.

terminate(_Resaon,_State)->
    ok.

%%--------------------------------------------------------------------------
%% Internal use functions
%%--------------------------------------------------------------------------

check_hash({Socket,Info_hash,Piece_number,Piece_length,Piece_hash,Received_length,ThePiece,My_id})->
    crypto:start(),
    Hash=crypto:sha(ThePiece),
    crypto:stop(),
    if
	(Hash==Piece_hash)->
	    %%send the piece and piece number to file manager to save it in file
	    {stop,{complete,Piece_number},{Socket,Info_hash,Piece_number,Piece_length,Piece_hash,Received_length,ThePiece,My_id}};
	true ->
	    {stop,{hash_not_match,Piece_number},{Socket,Info_hash,Piece_number,Piece_length,Piece_hash,Received_length,ThePiece,My_id}}
    end.

%-----------------------------------------------------------------------------
%  This functions call the propper function in Connection_handler Module
%-----------------------------------------------------------------------------
connect(_Ip,_Port)->
    ok.

handshake(_Socket,_Info_hash,_My_id)->
    % generate handshake bits
    % send request (Socket,HandShake bits)
    % receive a packet from peer
    % return ok / {error,Reason} based on received result
    ok.
send_request(_Socket,_Piece_number)->
    % generate piece request message
    % send request the message
    ok.

receive_packet(_Socket,_My_id)->
    ok.
%%--------------------------------------------------------------------------
%% User functions
%%--------------------------------------------------------------------------

start_link(IP,Port,Info_hash,Piece_number,Piece_length,Piece_hash,My_id)->
    gen_server:start_link({local,My_id},?MODULE,{IP,Port,Info_hash,Piece_number,Piece_length,Piece_hash,My_id},[]).

stop(My_id,Reason)->
    gen_server:cast(My_id,{stop,Reason}).
stop(My_id)->
    gen_server:cast(My_id,{stop,unknown_reason}).
    


