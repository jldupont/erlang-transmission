%% Author: Jean-Lou Dupont
%% Created: 2009-08-25
%% Description: Logging facility
%%
%% @doc
%%
%% == Configuration ==
%%  <ul>
%%	 <li>LogFileName: through 'start_link'</li>
%%  </ul>
%%
%% == Potential Configuration ==
%%  <ul>
%%	 <li>MaxLogSize</li>
%%   <li>MaxLogFiles</li>
%%  </ul>
%%	
%% == MESSAGE API ==
%%
%% In order to simplify interfacing to this logger module,
%% a message based API is provided. A client can send a 
%% tuple to the registered named process 'logger':
%%
%% ```
%%	{log, Severity, Msg, Params}
%% '''
%% Note that using this interface bypasses any potential
%% upstream policer functions.
%%
%% == Interfacing with Policers ==
%% 
%% Policer Modules should interface directly with this module.
%% 
%% == TODO ==
%%
%%  <ul>
%%	 <li>Auto-repair: if logging errors occur, automatically 'repair'</li>
%%  </ul>

-module(transmission_log).
%-compile({nowarn_unused_function, [loop, handle]}).

%% Keep this registered name in order for
%% other processes to easily interface to it.
-define(SERVER, log).

-define(SWITCH, transmission_hwswitch).

%% Local switch busses
-define(BUSSES, [log, sys, clock]).

%% Log unique identifier
-define(DEFAULT_LOG,  transmission).

%% Default dir (*nix)
-define(DEFAULT_DIR,  "/var/log/").

%% The system disk logger module
-define(LOG, disk_log).

%% Defaults
-define(DEFAULT_MAX_LOG_SIZE,  10*1000*1000).
-define(DEFAULT_MAX_LOG_FILES, 10).

-define(STAT_OPEN_ERROR, error_open_log).
-define(STAT_LOG_ERROR,  error_log_write).

-define(CTOOLS, mswitch_ctools).

%%
%% API Exported Functions
%%
-export([
		 start_link/0, start_link/1
		 ,get_server/0, get_busses/0
		 ,log/1, log/2, log/3
		 ,policed_log/1,policed_log/2, policed_log/3
		 ,close/0
		 ]).

%%
%% Config Functions
%%
-export([
		 defaults/0,
		 blacklist/0
		 ]).


%% LOCALS
-export([
		 loop/1
		,init/1
		 ]).

%% ----------------------      ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  API %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------      ------------------------------
get_server() ->	?SERVER.
get_busses() -> ?BUSSES.


%% @doc Default start
%%		Should be avoided because of probably unhelpful
%%		log file name.
%%
%% @spec start_link() -> {ok, Pid}
%%
start_link() ->
	Filepath=?DEFAULT_DIR++erlang:atom_to_list(?MODULE)++".log",
	run(?SERVER, Filepath).

%% @doc start_link
%%
%% @spec start_link([{logfilename,LogName}]) -> {ok, Pid}
%% where
%%	LogName = string()  %% absolute filename path
%%
start_link({logfilename, LogName}) when length(LogName)>0 ->
	run(?SERVER, LogName);

start_link({logfilename, LogName}) when length(LogName)==0 ->
	io:format("logger: invalid log filename: ~p~n", [LogName]),
	{error, logfilename};

start_link(Other) ->
	io:format("logger: invalid parameter: ~p~n", [Other]),
	{error, {invalid_parameter, Other}}.


%% ----------------------          ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%% LOG API  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% IMPORTANT: This API bypasses any active policers.
%% 
%% To integrate with upstream policers, please use messaging
%% on the HWSWITCH message switch:
%% 
%%	```
%%   {log, {Context, Severity, Msg, Params}}
%%  '''
%%
%% ----------------------          ------------------------------



%% @doc Log a message with 'info' severity
%%
%% @spec log(Msg) -> void()
%%
log(Msg) ->
	safe_send(info, Msg, []).

%% @doc Log a message with specific severity
%%
%% @spec log(Severity, Msg) -> void()
%%
log(Severity, Msg) ->
	safe_send(Severity, Msg, []).	

%% @doc Log a message with specific severity
%%		and append Params to Msg
%%
%% @spec log(Severity, Msg, Params) -> void()
%% where
%%	Severity = atom()
%%	Msg = atom() | string()
%%	Params = [atom() | string()]
%%
log(Severity, Msg, Params) ->
	safe_send(Severity, Msg, Params).



%% ----------------------           ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%% POLICER   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%% INTERFACE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------           ------------------------------

policed_log(Msg) ->
	safe_send(info, Msg, []).	

policed_log(Sev, Msg) ->
	safe_send(Sev, Msg, []).

policed_log(Sev, Msg, Ps) ->
	safe_send(Sev, Msg, Ps).	


%% ----------------------             ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%% SERVER LOOP %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------             ------------------------------

%% @private
run(Server, LogName) ->
	init(LogName),
	Pid=spawn_link(?MODULE, loop, [LogName]),
	register(Server, Pid),
	Pid ! {start, LogName},
	{ok, Pid}.


%% @private
loop(LogName) ->
	receive
		
		stop   -> handle(stop);
		reload -> handle(reload);

		{config, Version, Config} ->
			?CTOOLS:put_config(Version, Config);
		
		
		%%% LOCAL SWITCH RELATED %%%
		{hwswitch, From, Bus, Msg} ->
			handle({hwswitch, From, Bus, Msg});
		
		%% API - bypass policers
		%%
		{dolog, Severity, Msg, Params} ->
			maybe_dolog(Severity, Msg, Params);
		
		
		%% API - integration with policers
		{log, Severity, Msg, Params} -> 
			handle({log, Severity, Msg, Params});
		
		{start, LogName} -> 
			handle({start, LogName});
		
		Other -> 
			handle({log, critical, "logger: unhandled message: ", [Other]}) 
	end,
	loop(LogName).


%% ----------------------          ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%% HANDLERS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------         ------------------------------

%% Handler for direct communication
%% Normal API when using e.g. policer
%%
handle({log, Severity, Msg, Params}) ->
	maybe_dolog(Severity, Msg, Params);


handle({hwswitch, _From, sys, reload}) ->
	ok;

handle({hwswitch, _From, sys, {config, VersionInForce}}) ->
	?CTOOLS:do_config(?SWITCH, ?SERVER, VersionInForce);

handle({hwswitch, _From, sys, app.ready}) ->
	%io:format("log: app ready!!~n");
	not_supported;


handle({hwswitch, _From, sys, _}) ->
	not_supported;

handle({hwswitch, _From, clock, {tick.min, _Count}}) ->
	?CTOOLS:do_publish_config_version(?SWITCH, ?SERVER);

handle({hwswitch, _From, clock, {tick.sync, _Count}}) ->
	?CTOOLS:do_publish_config_version(?SWITCH, ?SERVER);	

handle({hwswitch, _From, clock, _}) ->
	not_supported;

%% Policer bypass point
%%
handle({hwswitch, _From, log, {_Context, {Severity, Msg, Params}}}) ->
	%io:format("log: handle: Msg[~p]~n", [Msg]),
	log_on_bypass(Severity, Msg, Params);

handle({hwswitch, _From, log, _}) ->
	not_supported;


handle({start, LogName}) ->
	put(logfilename, LogName),
	init(LogName);

handle(stop) ->
	close(),
	exit(normal);



%% @doc Exception... not much can be done...
%%
handle(Other) ->
	io:format("log: unhandled message[~p]~n", [Other]).



log_on_bypass(Sev, Msg, Params) ->
	Bypass=get('log.policer.bypass'),
	log_on_bypass(Bypass, Sev, Msg, Params).
	
log_on_bypass(true, Sev, Msg, Params) ->
	maybe_dolog(Sev, Msg, Params);

log_on_bypass(_, _Sev, _Msg, _Params) ->
	%io:format("log: not_bypassed: Msg[~p]~n", [Msg]),
	not_bypassed.






safe_send(Severity, Msg, Params) ->
	try
		?SERVER ! {dolog, Severity, Msg, Params}, ok
	catch
		_:_ -> error
	end.




maybe_dolog(Severity, Msg, Params) ->
	FlagVarName=make_atom_from_list([log,'.',Severity]),
	Flag=get(FlagVarName),
	maybe_dolog(Flag, Severity, Msg, Params).


maybe_dolog(false, _Severity, _Msg, _Params) -> blocked;
maybe_dolog(true, Severity, Msg, Params) ->
	dolog(Severity, Msg, Params);
maybe_dolog(undefined, Severity, Msg, Params) ->
	dolog(Severity, Msg, Params).



%%%%%%%%%%%% These functions interface directly
%%%%%%%%%%%% to the disk_log.


dolog(Severity, Msg, Params) ->
	Logger=get(log),
	dolog(Severity, Logger, Msg, Params).

dolog(Severity, undefined, Msg, []) ->
	{{Year,Month,Day},{Hour,Min,Sec}} = calendar:now_to_datetime(erlang:now()),
	io:format("~2B/~2B/~4B ~2B:~2.10.0B:~2.10.0B [~s] ~s:  ~p~n",[Day, Month, Year,Hour,Min,Sec, Severity, ?DEFAULT_LOG, Msg]),
	inc_stat(?STAT_LOG_ERROR);


dolog(Severity, undefined, Msg, Params) ->
	{{Year,Month,Day},{Hour,Min,Sec}} = calendar:now_to_datetime(erlang:now()),
	io:format("~2B/~2B/~4B ~2B:~2.10.0B:~2.10.0B [~s] ~s:  ~p ~p~n",[Day, Month, Year,Hour,Min,Sec, Severity, ?DEFAULT_LOG, Msg]++Params),
	inc_stat(?STAT_LOG_ERROR);

dolog(Severity, Logger, Msg, []) ->
	{{Year,Month,Day},{Hour,Min,Sec}} = calendar:now_to_datetime(erlang:now()),
	FMsg=io_lib:format("~2B/~2B/~4B ~2B:~2.10.0B:~2.10.0B [~s] ~s:  ~p~n",[Day, Month, Year,Hour,Min,Sec, Severity, ?DEFAULT_LOG, Msg]),
	Ret=?LOG:balog(Logger, FMsg),
	record_result(Ret);


dolog(Severity, Logger, Msg, Params) ->
	{{Year,Month,Day},{Hour,Min,Sec}} = calendar:now_to_datetime(erlang:now()),
	FMsg=io_lib:format("~2B/~2B/~4B ~2B:~2.10.0B:~2.10.0B [~s] ~s:  ~p ~p~n",[Day, Month, Year,Hour,Min,Sec, Severity, ?DEFAULT_LOG, Msg]++Params),
	Ret=?LOG:balog(Logger, FMsg),
	record_result(Ret).



record_result(ok) -> ok;

record_result(Result) ->
	io:format("Log error: ~p~n",[Result]),
	inc_stat(?STAT_LOG_ERROR).






%% Initializes the log file
%% - closes (or attempts) to close the current log file
%% - opens the specified log
%% - sets the process variables
%%
init(LogFileName) ->
	%% opens the log...
	Params=[
		{name,    ?DEFAULT_LOG}
		%%,{linkto, none}
		,{file,   LogFileName}
		,{type,   wrap}
		,{mode,   read_write}
		,{format, external}
		,{size,   {?DEFAULT_MAX_LOG_SIZE, ?DEFAULT_MAX_LOG_FILES}}
		,{repair, true}		   
	],
	Ret2=?LOG:open(Params),
	process_open(Ret2),
	%io:format("Log:init: completed, ret[~p]~n", [Ret2]),
	Ret2.

%% @doc Records log open errors
%%
process_open({error, _Reason}) ->
	inc_stat(?STAT_OPEN_ERROR);

process_open({ok, Log}) ->
	%io:format("process_open: log: ~p~n", [Log]),
	put(log, Log);

process_open({repaired, Log, _}) ->
	put(log, Log);

process_open(_) ->
	inc_stat(?STAT_OPEN_ERROR).

close() ->
	Log=get(log),
	?LOG:close(Log).


%% ----------------------         ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%% HELPERS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------         ------------------------------

%% @doc Increment a statistics counter Stat by 1
%%
%% @spec inc_stat(Stat) -> {Stat, NewCount}
%% where
%%	Stat = atom()
%%	NewCount = integer()
%%
inc_stat(Stat) ->
	inc_stat(Stat, 1).


%% @doc Increments an internal 'statistics' variable 'Stat' by 'Count'
%%      and returns the new count.
inc_stat(Stat, Count) when is_integer(Count) ->
	Value=getvar({stat, Stat}, 0),
	New=Value+Count,
	put({stat, Stat}, New),
	{Stat, New};

inc_stat(_,_) ->
	{error, invalid_count}.

getvar(VarName, Default) ->
	VarValue=get(VarName),
	getvar(VarName, VarValue, Default).

getvar(VarName, undefined, Default) ->
	put(VarName, Default),
	Default;

getvar(_VarName, VarValue, _Default) ->
	VarValue.



%% @doc Concatenates elements from the list
%%		into one atom.
%%
%% @spec make_atom_from_list(List) -> Result
%% where
%%	List = [atom() | string()]
%%  Result = list()
%%
make_atom_from_list(List) when is_list(List) ->
	{HeadPair, Rest}=head_pair(List, ''),
	make_atom_from_list(HeadPair, Rest).


make_atom_from_list([First,Second], []) ->
	concat_atoms(First, Second);


make_atom_from_list([First, Second], [Third|Rest]) ->
	Partial =concat_atoms(First, Second),
	Partial2=concat_atoms(Partial, Third),
	make_atom_from_list([Partial2|Rest]).



%% @doc Retrieves the first two elements of a list
%%
%% @spec head_pair(List, DefaultSecond) -> {Pair, Rest}
%% where
%%	List=list()
%%  DefaultSecond=atom() | string() | integer()
%%	Pair=list()
%%	Rest=list()
%%
head_pair([], _DefaultSecond) ->
	{[],[]};

head_pair(Liste, DefaultSecond) when is_list(Liste) ->
	[First|Rest] = Liste,
	head_pair(First, Rest, DefaultSecond).

head_pair(First, [], DefaultSecond) ->
	{[First, DefaultSecond], []};

head_pair(First, [Second|Rest], _DefaultSecond) ->
	{[First, Second], Rest}.


%% @doc Concatenates two atoms
%%
%% @spec concat_atoms(A1, A2) -> atom()
%%
concat_atoms(A1, A2) when is_atom(A1), is_atom(A2) ->
	L1=erlang:atom_to_list(A1),
	L2=erlang:atom_to_list(A2),
	erlang:list_to_atom(L1++L2);

concat_atoms(V1, V2) ->
	A1=to_atom(V1),
	A2=to_atom(V2),
	concat_atoms(A1, A2).

to_atom(V) when is_atom(V) -> V;
to_atom(V) when is_list(V) -> erlang:list_to_atom(V);
to_atom(V) when is_integer(V) ->
	L=erlang:integer_to_list(V),
	erlang:list_to_atom(L).


%% ----------------------          ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  CONFIG  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------          ------------------------------

%% @doc List of parameters which cannot be customized
%%
%% @spec blacklist() -> list()
%%
blacklist() ->
	[].

%% @doc List of default parameters for the module
%%
%% @spec defaults() -> list()
%%
defaults() ->
	[
	 {log.policer.bypass, optional, atom, true}
	
	 ,{log.debug, optional, atom, false}
	 ].

