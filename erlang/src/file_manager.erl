%%---------------------------------------------------------- 
%% @author Mahdi Abdinejadi <mehdi61b@gmail.com> @end
%% @version 0.2
%% @since Dec 1, 2010. 
%% @doc
%% This gen_server is responsible for writing file on hard disk. 
%% @end
%%----------------------------------------------------------
-module(file_manager).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
			terminate/2, code_change/3]).

%% Defult file location.
-define(LOCATION, "/home/mehdi/test").

%% File basic information 
-record(file, { name :: string() , size :: integer() , location :: string(), 
				piece_size :: integer() , piece_number :: integer() , 
				info = [] :: list()}).

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([start/5, start/4, stop/0, write_piece_data/3, new_file/4, new_file/5, 
		 get_info/1, update_state/4]).

%% gen_server callbacks

%% ====================================================================
%% External functions
%% ====================================================================

%% @doc 
%% Start gen_server after creating a file on the respective location. 
%% @end
-spec start(File_name :: string() , Size :: integer() , Piece_size :: integer(), 
			Piece_number :: integer() , Location :: string() )->
				{ok,pid()} | ignore | {error,term()}.

start(File_name, Size, Piece_size, Piece_number, Location)->
	error_logger:info_msg("start2 :", %% for test and logging 
		[File_name, Size, Piece_size, Piece_number, Location]),
	start({File_name, Size, Piece_size, Piece_number, Location}).
	

%% @doc Start gen_server after creating a file on default location. @end
-spec (start/4 :: ( string() , integer() , integer(), integer())-> 
		{ok,pid()} | ignore | {error,term()} ).
       
start(File_name, Size, Piece_size, Piece_number)->
	error_logger:info_msg("start1:",  %% for test and logging 
		[File_name, Size, Piece_size, Piece_number,?LOCATION ]),
	start(File_name, Size, Piece_size, Piece_number, ?LOCATION ).


%% @doc Stop gen_server. @end
-spec (stop/0 :: ()-> ok | {error, term()} ).

stop() ->
	case gen_server:cast(?MODULE, stop) of
		ok ->
			ok;
		Error ->
			Error
	end.

%% @doc 
%% Write a piece data on coresponding position of the file base on piece number
%% and send a message as call back (ok | Error). 
%% @end
-spec(write_piece_data/3 :: (string(), integer(), binary())->  
								ok | {error, term()} ).

write_piece_data(File_name, Peice_Num, Binary_Data)->
	gen_server:cast(?MODULE, 
					{write, File_name, Peice_Num, Binary_Data, self()}).


%% @doc Create a new file. @end
-spec (new_file/5 :: ( string() , integer() , integer(), integer(), string())-> 
		{ok,pid()} | ignore | {error,term()} ).

new_file(File_name, Size, Piece_size, Piece_number, Location)->
	gen_server:cast(?MODULE, {new_file, File_name, Size, Piece_size,
							   Piece_number, Location, self()}).


%% @doc Create a new file on default directory. @end
-spec (new_file/4 :: ( string() , integer() , integer(), integer())-> 
		{ok,pid()} | ignore | {error,term()} ).

new_file(File_name, Size, Piece_size, Piece_number)->
	gen_server:cast(?MODULE, {new_file, File_name, Size, Piece_size,
							   Piece_number, ?LOCATION ,self()}).


%% @doc 
%% Send a message to gen_server for getting a list of written piece numbers as
%% call back message. 
%% @end
-spec(get_info/1 :: (string())-> ok ).

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

handle_call(_Message, _From,State)-> %% not supported.
	{reply, ok, State}.	
		
%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

%% Send a sorted list of piece numbers that written in the hard drive. 
handle_cast({get_info, File_name, Pid}, State)->
	Result =  get_info(File_name, State),
	Pid ! Result,
	error_logger:error_report({"write file hande cast state$:",
				[{line,?LINE},{module,?MODULE}]}),%% for test and logging 
	{noreply, State};

%% stop the gen_server.
handle_cast(stop, State) -> 
	{stop, normal, State};

%% call write_data function and send ok or Error as result by message passing 
%% and update written pieces list.
handle_cast({write, File_name, Peice_Num, Binary_Data, Pid}, State)->
	try write_data({File_name, Peice_Num, Binary_Data}, State) of
		{ok, New_state} ->
			error_logger:error_report({"write file hande cast state$:", State ,
				[{line,?LINE},{module,?MODULE}]}),%% for test and logging
			Pid ! ok,
			{noreply, New_state}
	catch
		Error->
			Pid ! Error,
			{noreply, State}
	end;

%% call generate_file function and send ok or Error as result by 
%% message passing; Also update the State.
handle_cast({new_file, 
			 File_name, Size, Piece_size, Piece_number, Location, Pid}, State)->
	New_state =  [ #file{name = File_name, size = Size, piece_size = Piece_size, 
					piece_number = Piece_number, location = Location }| State],
	try generate_file(File_name, Size, Location) of
		ok->
			Pid ! ok,
			{noreply, New_state}			
	catch 
		Error->
			Pid! Error,
			error_logger:error_report({"An error occurred",
				Error, [{line,?LINE},{module,?MODULE}]}),%% for test and logging
			{reply, Error, State}
	end.
%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(_Info, State) -> %% not supported.
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
code_change(_OldVsn, State, _Extra) -> %% not supported.
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

%% call generate_file and if to return ok the start genserver, otherwise it 
%% return an Error.
-spec(start({string(), integer(), integer(), integer(), string()})-> 
			ok | {error, term()}).

start({Name, Size, Piece_size, Piece_number, Location})->
			error_logger:info_msg("start{  }:", % for test and logging 
				[Name, Size, Piece_size, Piece_number, Location]),
	try generate_file(Name, Size, Location) of
		ok ->
			State = [#file{ name = Name, size = Size, location = Location ,
				piece_size = Piece_size, piece_number = Piece_number }],
			io:format("test$$~p$$", [State ]), % for test only
			case gen_server:start({local, ?MODULE}, ?MODULE, [State] , []) of
				{ok, Pid} ->
					{ok, Pid};
				{error, Reason} ->
					error_logger:error_report({"An error occurred", Reason,
						 [{line,?LINE},{module,?MODULE}]}),%for test and logging
					{error, Reason}
			end
	catch 
		Error->
			error_logger:error_report({"An error occurred", Error, 
				[{line,?LINE},{module,?MODULE}]}),% for test and logging
			Error
	end.


%% Create a file with respected size; basiclly it only write a bite at the last
%% byte of the file; it return ok or crash.
-spec(generate_file/3 :: (string(), integer(), string())-> ok ).

generate_file(Name , Size, Location)->
	ok = file:write_file(Location ++"/"++ Name , <<>>),
	{ok, IoDevice} = file:open( Location ++ Name , [raw , write]),
	{ok, _ } = file:position(IoDevice, Size-1),
	ok = file:write(IoDevice, <<0>> ),
	ok = file:close(IoDevice),
	ok.


%% It write piece data; it caculate the postion of piece data base on 
%% piece number and add piece number to info list of the state.
-spec(write_data/2 :: ({string(), integer(), binary()},list())-> {ok, term()}).

write_data({File_name, Peice_Num, Binary_Data}, State)->
	File_record = extract_record(State , File_name),
	{ok, IoDevice} = file:open( File_record#file.location ++"/"
							  ++ File_name , [raw , write]),
	{ok, _ } = file:position(IoDevice, (Peice_Num - 1) * 
								File_record#file.piece_size),
	ok = file:write(IoDevice, Binary_Data ),
	ok = file:close(IoDevice),
	{ok, update_state(State, File_record, Peice_Num, [])}. 


%% Return a sorted list of written piece numbers for coresponding file.
-spec(get_info/2 :: (string(), list())-> list() ).
get_info(File_name, State)->
	File_record = extract_record(State, File_name),
	lists:sort(File_record#file.info).


%% Notic: base on trust internal data convention, this function crash if it get
%% wrong information. 
%% It update state of the gen_server by adding piece number to coressponding 
%% file record on the State.
-spec(update_state(list(), term(), integer(), list())-> list() ).

update_state([Record | Rest], File_record, Peice_Num, Acc)->
	case Record#file.name == File_record#file.name of
		true ->
			New_info = Record#file.info ++ [Peice_Num],
			error_logger:error_report({"udate data $new_info$:", New_info , 
					[{line,?LINE},{module,?MODULE}]}), % for test and logging
			New_record = Record#file{info = New_info},
			error_logger:error_report({"udate data $new_record$:", New_record , 
					[{line,?LINE},{module,?MODULE}]}), % for test and logging
			lists:flatten(Rest ++ [New_record] ++ Acc);
		false -> 
			error_logger:error_report({"udate data $Record$:", Record , 
					[{line,?LINE},{module,?MODULE}]}), % for test and logging
			update_state(Rest, File_record, Peice_Num, [Record|Acc])
	end;

update_state([], _File_name, _Peice_Num, Acc)->
	Acc.


%% Extract respective record to file name form list of records(state) and 
%% returns the record or not_exist.
-spec(extract_record(list(), string())->  term() | not_exist ).

extract_record([Record | Rest], File_name)->
	error_logger:error_report({"An error occurred in extract_record",
				 [{line,?LINE},{module,?MODULE}]}), % for test and logging
	case Record#file.name == File_name of
		true -> Record;
		false -> extract_record(Rest, File_name)
	end;

extract_record([], _File_name)->
	not_exist .
