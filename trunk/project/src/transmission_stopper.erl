%% Author: Jean-Lou Dupont
%% Created: 2009-09-08
%% Description: Torrent stopper
%%
-module(transmission_stopper).

-define(SERVER,  stopper).
-define(SWITCH,  transmission_hwswitch).
-define(BUSSES,  [sys, clock, notif, data]).
-define(TOOLS,   mswitch_tools).
-define(CTOOLS,  mswitch_ctools).
-define(MSWITCH, mswitch).
-define(CLIENT,  transmission_client).

-define(STATE_COMPLETED, 8).


%%
%% API Functions
%%
-export([
		 start_link/0
		,stop/0
		,get_server/0
		,get_busses/0
		]).

%%
%% LOCAL Functions
%%
-export([
		 loop/0
		 ]).

%%
%% Config Functions
%%
-export([
		 defaults/0,
		 blacklist/0
		 ]).


%% ----------------------              ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  Management  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------              ------------------------------
get_server() ->	?SERVER.
get_busses() -> ?BUSSES.


start_link()->
	Pid=spawn_link(?MODULE, loop, []),
	register(?SERVER, Pid),
	{ok, Pid}.


stop() ->
	try 
		?SERVER ! stop,  ok
	catch
		_:_ ->	{error, cannot_stop}
	end.



%% ----------------------         ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  LOCAL  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------         ------------------------------
loop() ->
	receive
		
		%%% MANAGEMENT RELATED %%%
		{config, Version, Config} ->
			?CTOOLS:put_config(Version, Config);
		
		stop ->
			exit(normal);
	
		%%% LOCAL SWITCH RELATED %%%
		{hwswitch, From, Bus, Msg} ->
			handle({hwswitch, From, Bus, Msg});
	
		%%% TRANSMISSION API related
		%%% ------------------------
		{http, {RequestId, {error, Reason}}} ->
			erase({requestid, RequestId}),
			erase({method, RequestId}),
			handle_api_error(Reason);

		{http, {RequestId, Result}} ->
			handle_api_response(RequestId, Result);
		
		
		Other ->
			log(warning, "stopper: unexpected message: ", [Other])
	end,
	loop().

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
	%io:format("stopper: rx sys msg[~p]~n", [Msg]);
	not_supported;


handle({hwswitch, _From, notif, {torrent, _, Msg} }) ->
	maybe_stop_torrent(Msg);

handle({hwswitch, _From, notif, _}) ->
	not_supported;

handle({hwswitch, _From, data, Data}) ->
	grab_data(Data);


handle(Other) ->
	log(warning, "stopper: Unexpected message: ", [Other]).



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


hr(_Rid, Rd, "torrent-remove", _Result, _Code, _Headers, Body) ->
	{Name, Id, DL} = Rd,	
	try
		{ok, D}=?CLIENT:decode(Body),
		{_A, R}=?CLIENT:extract(D, arguments),
		log(info, "remove report: {Name, Result} ", [[Name, R]])
	catch
		_X:_Y ->
			log(error, "remove exception: {Name, Id, DownloadDir}", [[Name, Id, DL]]),
			error
	end;


hr(_Rid, _Rd, Method, _Result, _Code, _Headers, _Body) ->
	log(error, "unhandled method: ", [Method]).


maybe_grab_sid({_, Sid}) ->
	clog(api.session.id, info, "session id: ", [Sid]),
	put(session.id, Sid);

maybe_grab_sid(_) ->
	ok.


grab_data({session.id, Sid}) ->	put(session.id, Sid);
grab_data(_) ->	not_supported.


%% ----------------------          ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  LOCALS  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------          ------------------------------

maybe_stop_torrent(Msg) ->
	State=get_state(),
	maybe_stop_torrent(State, Msg).

maybe_stop_torrent(working, Msg) ->
	Autostop=get(stopper.autostop),
	maybe_stop(Autostop, Msg);

maybe_stop_torrent(_, _Msg) ->
	suspended.

maybe_stop(true, {Name, Id, ?STATE_COMPLETED, DL}) ->
	StrId=to_list(Id),
	log(info, "attempting to stop: name<~p> id<~p> ~n~n", [[Name, StrId]]),	
	SessionId=get(session.id),
	?CLIENT:request({Name, Id, DL}, SessionId, torrent.remove, StrId, []);


maybe_stop(_, _Msg) ->
	no_autostop.
	

%% ----------------------          ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  HELPERS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------          ------------------------------
to_list(I) when is_integer(I) ->
	erlang:integer_to_list(I);

to_list(A) when is_atom(A) ->
	erlang:atom_to_list(A);

to_list(F) when is_float(F) ->
	erlang:float_to_list(F);

to_list(L) when is_list(L) ->
	L;

to_list(E) -> E.



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
	 %% Priority level for messages sent on the 'notif' bus through Mswitch
	 {stopper.autostop,  optional, atom,  true}
	 ].

