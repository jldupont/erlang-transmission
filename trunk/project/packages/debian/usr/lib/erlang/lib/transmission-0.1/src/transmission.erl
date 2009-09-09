%% Author: Jean-Lou Dupont
%% Created: 2009-08-07
%% Description: Transmission client library
%%
%% Session-id:  recovered when 409 HTTP code received
%%

-module(transmission).

%%
%% Defines
%%
-define(SERVER,     transmission).
-define(SWITCH,     transmission_hwswitch).
-define(BUSSES,     [clock, sys]).
-define(RPCTIMEOUT, 2000).
-define(CLIENT,     transmission_client).
-define(TOOLS,      mswitch_tools).
-define(CTOOLS,     mswitch_ctools).
-define(RPC,        transmission_rpc).
-define(DAEMON_RPC_TIMEOUT, 2000).


%%
%% Exported Functions
%%
-export([
		 start_link/0,
		 stop/0,
		 
		 %% API
		 get_server/0
		,get_busses/0
	    ,defaults/0
	    ,blacklist/0
	   
		,daemon_api/2
		 ]).

-export([
		 loop/0 
		 ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Exported Functions - not really part of API per-se
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_link() ->
	Pid=spawn_link(?MODULE, loop, []),
	register(?SERVER, Pid),
	%io:format("~p: erl pid[~p] os pid[~p]~n", [?MODULE, Pid, os:getpid()]),
	{ok, Pid}.


stop() -> ?SERVER ! stop.



%% ----------------------            ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%% DAEMON RPC %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------            ------------------------------

%% Messages are sent to the server loop in this module
%%
daemon_api(ReplyContext, Command) ->
	case ?RPC:rpc_validate_command(Command) of
		true ->
			?RPC:rpc(ReplyContext, Command);
		_ ->
			%?MNG:inc_stat(error_daemon_api_invalid_command),
			{error, invalid_command}
	end.


%% ----------------------            ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%% DAEMON API %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------            ------------------------------






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ADMIN API Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_server() ->	?SERVER.
get_busses() -> ?BUSSES.





%% ----------------------              ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%% SERVER LOOP  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------              ------------------------------
loop() ->
	receive
		
		%% RPC bridge
		%%
		%% Messages ending up here are (usually ;-)
		%% sent through using the 'rpc' function.
		%%
		{rpc, ReplyTo, {FromNode, ReplyContext, Q}} ->
			?RPC:handle_rpc(ReplyTo, FromNode, ReplyContext, Q);
	
		
		%%%% CONFIGURATION MANAGEMENT
		
		{config, Version, Config} ->
			?CTOOLS:put_config(Version, Config);
		
		stop ->
			exit(normal);
	
		%%% LOCAL SWITCH RELATED %%%
		%%% --------------------
		{hwswitch, From, Bus, Msg} ->
			handle({hwswitch, From, Bus, Msg});

		
		%%% TRANSMISSION API related
		%%% ------------------------
		{http, {RequestId, {error, Reason}}} ->
			%ReturnDetails=get({requestid, RequestId}),
			%%Method=get({method, RequestId}),
			erase({requestid, RequestId}),
			erase({method, RequestId}),
			%%io:format("got: Method[~p]~n",[Method]),
			handle_api_error(Reason);
			

		
		%% Result = {{HttpVersion, HttpCode, HttpResponseCode}, [Headers], ResponseBody}
		%% HttpVersion = string()         (eg. "HTTP/1.1")
		%% HttpCode = integer()           (eg. "200")
		%% HttpResponseCode = string()    (eg. "OK")
		%% Headers = {key, value}, {key, value} ...
		%% ResponseBody = string()
		{http, {RequestId, Result}} ->
			handle_api_response(RequestId, Result);

	
		poll.timer.expired ->
			do_polling();
		
		_Other ->
			log(critical, "unhandled message: ", [_Other])
	
	end,
	loop().


%% ----------------------            ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%    API     %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%  HANDLERS  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------            ------------------------------

handle_api_error(Reason) ->
	State=get(request.state),
	handle_api_error(State, Reason).

%% The BT transmission is probably off-line
handle_api_error(undefined, Reason) ->
	put(request.state, error),
	clog(api.error, error, "Request error, reason: ", [Reason]);

%% The BT transmission is probably off-line
handle_api_error(success, Reason) ->
	put(request.state, error),
	clog(api.error, error, "Request error, reason: ", [Reason]);

%% Squelch repeating errors
handle_api_error(error, _Reason) ->
	ok.



handle_api_response(Rid, Result) ->
	%% for squelching functionality
	put(request.state, success),
	
	Rd=get({requestid, Rid}),
	%%io:format("hresponse: rd: ~p~n", [Rd]),	
	Method=get({method, Rid}),
	erase({requestid, Rid}),
	erase({method, Rid}),
	Code=?TOOLS:http_extract(Result, http.code),
	Headers=?TOOLS:http_extract(Result, headers),
	Body=?TOOLS:http_extract(Result, body),
	hr(Rid, Rd, Method, Result, Code, Headers, Body).


%% Grab the session Id for future requests
hr(_Rid, _Rd, _Method, _Result, 409, Headers, _) ->
	Result=?TOOLS:kfind("x-transmission-session-id", Headers, not_found),
	maybe_grab_sid(Result);


hr(_Rid, _Rd, "torrent-get", _Result, _Code, _Headers, Body) ->
	try
		{ok, D}=?CLIENT:decode(Body),
		Result=?CLIENT:extract(D, arguments),
		%io:format("hr: result: ~p~n~n", [Result]),
		{A, _R}=Result,
		Torrents=?CLIENT:extract(A, torrents),
		process_torrents(Torrents)
	catch
		X:Y ->
			%io:format("X<~p> Y<~p>~n", [X,Y]),
			error
	end;


hr(_Rid, _Rd, Method, _Result, _Code, _Headers, _Body) ->
	log(error, "unhandled method: ", [Method]).


process_torrents(Torrents) when is_list(Torrents), length(Torrents)>0 ->
	[Torrent|Rest]=Torrents,
	Name=?CLIENT:extract(torrent, Torrent, name),
	Id=?CLIENT:extract(torrent, Torrent, id),
	Status=?CLIENT:extract(torrent, Torrent, status),
	DL=?CLIENT:extract(torrent, Torrent, downloadDir),
	%io:format("torrent: name<~p> id<~p> status<~p> ~n~n", [Name, Id, Status]),
	maybe_notif_torrent(Name, Id, Status, DL),
	process_torrents(Rest);

process_torrents([]) ->	finished;
process_torrents(_)  -> nothing_todo.


maybe_notif_torrent(error, _, _, _) ->
	invalid_torrent;

maybe_notif_torrent(Name, Id, Status, DL) ->
	Torrent=get({torrent, Name}),
	maybe_notif_torrent2(Name, Id, Status, DL, Torrent).
	
% first time around 
maybe_notif_torrent2(Name, Id, Status, DL, undefined) ->
	MinCount=get_clock_min(),
	put({torrent, Name}, {Id, Status, DL, MinCount}),
	log(info, "new torrent {Name, Id, DownloadDir}: ", [[Name, Id, DL]]),
	?SWITCH:publish(notif, {torrent, 3, {Name, Id, Status, DL}});

maybe_notif_torrent2(Name, Id, Status, DL, {_, PreviousStatus, _DL, Count}) ->
	CurrentCount=get(clock.min),

	case Status == PreviousStatus of
		 true  ->
			maybe_repeat_notif(CurrentCount, Count, Name, Id, Status, DL);			 
		 false ->
			 put({torrent, Name}, {Id, Status, DL, Count}),
			 do_notif(Name, Id,Status, DL)
	end;

maybe_notif_torrent2(_, _, _, _, Other) ->
	log(critical, "notif exception: ", [Other]).


do_notif(Name, Id, Status, DL) ->
	 log(info, "torrent status change {Name, Id, Status}: ", [[Name, Id, Status]]),
	 ?SWITCH:publish(notif, {torrent, 3, {Name, Id, Status, DL}}).

	

maybe_repeat_notif(CurrentCount, Count, Name, Id, Status, DL) ->
	RI=get(transmission.repeat.status.interval),
	Delta=CurrentCount-Count,
	case Delta>RI of
		true ->
			%% reset counter
			put({torrent, Name}, {Id, Status, DL, CurrentCount}),
			do_notif(Name, Id, Status, DL);
		_ -> 
			do_nothing
	end.



maybe_grab_sid({_, Sid}) ->
	clog(api.session.id, info, "session id: ", [Sid]),
	?SWITCH:publish(data, {session.id, Sid}),
	put(session.id, Sid);

maybe_grab_sid(_) ->
	ok.

get_clock_min() ->
	MinCount=get(clock.min),
	case MinCount of
		undefined ->
			0;
		Count -> Count
	end.




%% poll for status of Torrents
%%
do_polling() ->
	SessionId=get(session.id),
	State=get_state(),
	maybe_do_polling(State, SessionId).

maybe_do_polling(working, SessionId) ->
	?CLIENT:request(undefined, SessionId, torrent.get, [], []);

maybe_do_polling(_, _SessionId) ->
	suspended.


%% ----------------------            ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  HWSWITCH  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%  HANDLERS  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------            ------------------------------

handle({hwswitch, _From, clock, {tick.min, Count}}) ->
	put(clock.min, Count),
	?CTOOLS:do_publish_config_version(?SWITCH, ?SERVER);

handle({hwswitch, _From, clock, {tick.sync, _Count}}) ->
	?CTOOLS:do_publish_config_version(?SWITCH, ?SERVER);	

handle({hwswitch, _From, clock, _}) ->
	not_supported;

handle({hwswitch, _From, sys, app.ready}) ->
	do_app_ready();


handle({hwswitch, _From, sys, suspend}) ->
	set_state(suspended);

handle({hwswitch, _From, sys, resume}) ->
	set_state(working);

handle({hwswitch, _From, sys, {config, VersionInForce}}) ->
	put(config.version.inforce, VersionInForce);


%% A module advertises its configuration version
handle({hwswitch, _From, sys, {mod.config, Module, Version}}) ->
	put({mod.version, Module}, Version);


handle({hwswitch, _From, sys, _Msg}) ->
	%io:format("app: rx sys msg[~p]~n", [Msg]);
	not_supported;



handle(Other) ->
	log(warning, "app: Unexpected message: ", [Other]).



%% ----------------------          ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  HELPERS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------          ------------------------------
do_app_ready() ->
	TRef=get(polling.timer),
	set_timer(TRef).

set_timer(TRef) ->
	%% won't crash even on 'undefined'
	timer:cancel(TRef),
	Poll=get(transmission.poll.interval),
	Result=timer:send_interval(Poll, poll.timer.expired),
	case Result of
		{error, Reason} ->
			log(critical, "cannot initialize polling timer, reason: ", [Reason]);
		{ok, TRef2} ->
			put(polling.timer, TRef2)
	end.

		




set_state(State) ->
	put(state, State).

get_state() ->
	State=get(state),
	case State of
		undefined -> working;   %start in 'working' state
		working   -> working;
		_         -> suspended
	end.






%% ----------------------          ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  LOGGER  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------          ------------------------------

%%log(Severity, Msg) ->
%%	log(Severity, Msg, []).

log(Severity, Msg, Params) ->
	?SWITCH:publish(log, {?SERVER, {Severity, Msg, Params}}).

%clog(Ctx, Sev, Msg) ->
%	?SWITCH:publish(log, {Ctx, {Sev, Msg, []}}).

clog(Ctx, Sev, Msg, Ps) ->
	?SWITCH:publish(log, {Ctx, {Sev, Msg, Ps}}).

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
	 %% poll interval (in ms)
	 {transmission.poll.interval,          optional, int,  30*1000}
	,{transmission.poll.interval.min,      optional, int,  1*1000}
	,{transmission.poll.interval.max,      optional, int,  10*60*1000}
	 
	,{transmission.repeat.status.interval,     optional, int,  1*60*1000}
	,{transmission.repeat.status.interval.min, optional, int,  1*60*1000}
	,{transmission.repeat.status.interval.max, optional, int,  10*60*1000}
	
	%% Write a '.completed' file when a download is finished
	%,{transmission.write.completed, optional, atom, true}
	 ].

