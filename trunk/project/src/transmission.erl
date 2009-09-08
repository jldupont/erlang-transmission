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
-define(BUSSES,     [sys]).
-define(RPCTIMEOUT, 2000).
-define(CLIENT,     transmission_client).
-define(TOOLS,      mswitch_tools).
-define(CTOOLS,     mswitch_ctools).
-define(RPC,        transmission_rpc).


%%
%% Exported Functions
%%
-export([
		 start_link/1,
		 stop/0,
		 
		 %% API
		 get_server/0
		,get_busses/0
	    ,defaults/0
	    ,blacklist/0
	   
		,daemon_api/2
		,req/4
		 ]).

-export([
		 loop/1, 
		 reply/2,
		 doreq/4
		 ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Exported Functions - not really part of API per-se
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_link(Args) ->
	Pid=spawn_link(?MODULE, loop, [Args]),
	register(?SERVER, Pid),
	%io:format("~p: erl pid[~p] os pid[~p]~n", [?MODULE, Pid, os:getpid()]),
	{ok, Pid}.


stop() -> ?SERVER ! stop.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% RPC Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
daemon_api(status) ->
	{pid, os:getpid()};

daemon_api(reload) ->
	?SWITCH:publish(sys, reload);

daemon_api(Cmd) ->
	{command_invalid, Cmd}.



req(ReplyDetails, Method, MandatoryParams, OptionalParams) ->
	rpc({request, ReplyDetails, Method, MandatoryParams, OptionalParams}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ADMIN API Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_server() ->	?SERVER.
get_busses() -> ?BUSSES.



%% ----------------------                   ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%% DAEMON MANAGEMENT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------                   ------------------------------


daemon_api(ReplyContext, Command) ->
	case ?RPC:rpc_validate_command(Command) of
		true ->
			?RPC:rpc(ReplyContext, Command);
		_ ->
			%?MNG:inc_stat(error_daemon_api_invalid_command),
			{error, invalid_command}
	end.





%% ----------------------              ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%% SERVER LOOP  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------              ------------------------------
loop(Args) ->
	receive
		
		%% RPC bridge
		%%
		%% Messages ending up here are (usually ;-)
		%% sent through using the 'rpc' function.
		%%
		{rpc, ReplyTo, {FromNode, ReplyContext, Q}} ->
			?RPC:handle_rpc(ReplyTo, FromNode, ReplyContext, Q);
	
		
		{config, Version, Config} ->
			?CTOOLS:put_config(Version, Config);
		
		stop ->
			exit(normal);
	
		%%% LOCAL SWITCH RELATED %%%
		{hwswitch, From, Bus, Msg} ->
			handle({hwswitch, From, Bus, Msg});

		
		{http, {RequestId, {error, Reason}}} ->
			ReturnDetails=get({requestid, RequestId}),
			%%Method=get({method, RequestId}),
			erase({requestid, RequestId}),
			erase({method, RequestId}),
			%%io:format("got: Method[~p]~n",[Method]),
			reply(ReturnDetails, {error, Reason});

		
		%% Result = {{HttpVersion, HttpCode, HttpResponseCode}, [Headers], ResponseBody}
		%% HttpVersion = string()         (eg. "HTTP/1.1")
		%% HttpCode = integer()           (eg. "200")
		%% HttpResponseCode = string()    (eg. "OK")
		%% Headers = {key, value}, {key, value} ...
		%% ResponseBody = string()
		{http, {RequestId, Result}} ->
			?CLIENT:hresponse(RequestId, Result)

	
	end,
	loop(Args).



%% @private
reply(undefined, Message) ->
	io:format("~p:reply(~p)~n", [?MODULE, Message]),
	Message;

%% @private
reply({From, Context}, Message) ->
	From ! {Context, Message}.



%% ----------------------            ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  HANDLERS  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------            ------------------------------

handle({hwswitch, _From, clock, {tick.min, _Count}}) ->
	?CTOOLS:do_publish_config_version(?SWITCH, ?SERVER);

handle({hwswitch, _From, clock, {tick.sync, _Count}}) ->
	?CTOOLS:do_publish_config_version(?SWITCH, ?SERVER);	

handle({hwswitch, _From, clock, _}) ->
	not_supported;


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
set_state(State) ->
	put(state, State).

%get_state() ->
%	State=get(state),
%	case State of
%		undefined -> working;   %start in 'working' state
%		working   -> working;
%		_         -> suspended
%	end.



doreq(ReplyDetails, Method, MandatoryParams, OptionalParams) ->
	reply(ReplyDetails, request_done).




hresponse(Rid, Result) ->
	Rd=get({requestid, Rid}),
	%%io:format("hresponse: rd: ~p~n", [Rd]),	
	Method=get({method, Rid}),
	erase({requestid, Rid}),
	erase({method, Rid}),
	Code=?TOOLS:http_extract(Result, http.code),
	Headers=?TOOLS:http_extract(Result, headers),
	Body=?TOOLS:http_extract(Result, body),
	hr(Rid, Rd, Method, Result, Code, Headers, Body).


hr(_Rid, Rd, _Method, _Result, 409, Headers, _) ->
	Sid=?TOOLS:kfind("x-transmission-session-id", Headers, not_found),
	reply(Rd, {session_id, Sid});

hr(_Rid, Rd, _Method, _Result, Code, Headers, Body) ->
	reply(Rd, {response, Code, Headers, Body}).





%% ----------------------          ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  LOGGER  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------          ------------------------------

%%log(Severity, Msg) ->
%%	log(Severity, Msg, []).

log(Severity, Msg, Params) ->
	?SWITCH:publish(log, {?SERVER, {Severity, Msg, Params}}).

clog(Ctx, Sev, Msg) ->
	?SWITCH:publish(log, {Ctx, {Sev, Msg, []}}).

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
	 {transmission.poll.interval,    optional, int,  30*1000}
	 
	 %% Priority level for messages sent on the 'notif' bus through Mswitch
	 ,{transmission.notif.priority,  optional, int,  2}
	
	%% Write a '.completed' file when a download is finished
	,{transmission.write.completed, optional, atom, true}
	 ].

