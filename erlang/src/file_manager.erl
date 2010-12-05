%%---------------------------------------------------------- 
%% @author mahdi abdinejadi <mehdi61b@gmail.com; max.jadi@pontarius.org> @end
%% @version 0.1
%% @since Dec 1, 2010. 
%% @doc this gen_server is responsible for writing file on hard disk. 
%% @end
%%----------------------------------------------------------
-module(file_manager).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% -record(state, { files = [] :: [ file() ] }).

-record(file, { name :: string() , size :: integer() , location = "/home" :: string(), 
				piece_size :: integer() , piece_number :: integer() , info = [] :: list()}).
%% -type( file() :: #file{} ).
%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([start/5, start/4, stop/0, write_piece_data/3, new_file/4, new_file/5, get_info/1, update_state/4]).

%% gen_server callbacks

%% ====================================================================
%% External functions
%% ====================================================================
start(File_name, Size, Piece_size, Piece_number, Location)->
	start({File_name, Size, Piece_size, Piece_number, Location}).

start(File_name, Size, Piece_number, Piece_size)->
	start(File_name, Size, Piece_number, Piece_size, #file.location ).

stop() ->
	case gen_server:cast(?MODULE, stop) of
		ok ->
			ok;
		Error ->
			Error
	end.
write_piece_data(File_name, Peice_Num, Binary_Data)->
	gen_server:call(?MODULE, {write, File_name, Peice_Num, Binary_Data}).

new_file(File_name, Size, Piece_size, Piece_number, Location)->
	gen_server:call(?MODULE, {new_file, File_name, Size, Piece_size, Piece_number, Location}).
new_file(File_name, Size, Piece_size, Piece_number)->
	gen_server:call(?MODULE, {new_file, File_name, Size, Piece_size, Piece_number, #file.location}).

get_info(File_name)->
	gen_server:cast(?MODULE, {get_info, File_name, self()}).
%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%% 	start({File_name, Size, Piece_size, Piece_number, Location}, 
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([State]) ->
    {ok, State }.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

handle_call({write, File_name, Peice_Num, Binary_Data}, _From, State)->
	try write_data({File_name, Peice_Num, Binary_Data}, State) of
		{ok, Files_list} ->
			error_logger:error_report({"write file hande call state$:", State , [{line,?LINE},{module,?MODULE}]}),
			{reply, ok, Files_list}
	catch
		Error->
			{reply, Error, State}
	end;

handle_call({new_file, File_name, Size, Piece_size, Piece_number, Location}, _From, State)->
	New_state =  [ #file{name = File_name, size = Size, piece_size = Piece_size, 
									   piece_number = Piece_number, location = Location }| State],
	try generate_file(File_name, Size, Location) of
		ok->
			{reply, ok, New_state}			
	catch 
		Error->
			error_logger:error_report({"An error occurred", Error, [{line,?LINE},{module,?MODULE}]}),
			{reply, Error, State}
	end. 
		
%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

handle_cast({get_info, File_name, Pid}, State)->
	Result =  get_info(File_name, State),
	Pid ! Result,
	io:format("get_info result: ~p \n",[Result]),
	{noreply, State};
handle_cast(stop, State) -> 
	{stop, normal, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)ok = file:close(IoDevice)
%% --------------------------------------------------------------------
terminate(Reason, State) ->
    {error, Reason, State}.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
start({Name, Size, Piece_size, Piece_number, Location})->
			io:format("test~p$$~p", [Location , Name ]),
	try generate_file(Name, Size, Location) of
		ok ->
			State = [#file{ name = Name, size = Size, location = Location ,
											   piece_size = Piece_size, piece_number = Piece_number }],
			io:format("test$$~p$$", [State ]),
			case gen_server:start({local, ?MODULE}, ?MODULE, [State] , []) of
				{ok, _Pid} ->
					ok;
				{error, Reason} ->
					error_logger:error_report({"An error occurred", Reason, [{line,?LINE},{module,?MODULE}]})
			end
	catch 
		Error->
			error_logger:error_report({"An error occurred", Error, [{line,?LINE},{module,?MODULE}]}),
			Error
	end.

generate_file(Name , Size, Location)->
	ok = file:write_file(Location ++"/"++ Name , <<>>),
	{ok, IoDevice} = file:open( Location ++ Name , [raw , write]),
	{ok, _ } = file:position(IoDevice, Size-1),
	ok = file:write(IoDevice, <<0>> ),
	ok = file:close(IoDevice),
	ok.

write_data({File_name, Peice_Num, Binary_Data}, State)->
	File_record = extract_record(State , File_name),
	{ok, IoDevice} = file:open( File_record#file.location ++"/"++ File_name , [raw , write]),
	{ok, _ } = file:position(IoDevice, (Peice_Num - 1) * File_record#file.piece_size),
	ok = file:write(IoDevice, Binary_Data ),
	ok = file:close(IoDevice),
	{ok, update_state(State, File_record, Peice_Num, [])}. 

get_info(File_name, State)->
	File_record = extract_record(State, File_name),
	File_record#file.info.

%% base on trust internal data convention, this function crash if it get wrong information.  	
update_state([Record | Rest], File_record, Peice_Num, Acc)->
	case Record#file.name == File_record#file.name of
		true ->
			New_info = Record#file.info ++ [Peice_Num],
			error_logger:error_report({"udate data $new_info$:", New_info , [{line,?LINE},{module,?MODULE}]}),
			New_record = Record#file{info = New_info},
			error_logger:error_report({"udate data $new_record$:", New_record , [{line,?LINE},{module,?MODULE}]}),
			lists:flatten(Rest ++ [New_record] ++ Acc);
		false -> 
			error_logger:error_report({"udate data $Record$:", Record , [{line,?LINE},{module,?MODULE}]}),
			update_state(Rest, File_record, Peice_Num, [Record|Acc])
	end;
update_state([], _File_name, _Peice_Num, Acc)->
	Acc.

extract_record([Record | Rest], File_name)->
	error_logger:error_report({"An error occurred $extract_record$ in extract_record", Record#file.name, [{line,?LINE},{module,?MODULE}]}),
	case Record#file.name == File_name of
		true -> Record;
		false -> extract_record(Rest, File_name)
	end;
extract_record([], _File_name)->
	not_exist .
