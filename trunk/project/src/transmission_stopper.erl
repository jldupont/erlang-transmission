%% Author: Jean-Lou Dupont
%% Created: 2009-09-08
%% Description: Torrent stopper
%%
-module(transmission_stopper).

-define(SERVER,  stopper).
-define(SWITCH,  transmission_hwswitch).
-define(BUSSES,  [sys, clock, notif]).
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
	%io:format("app: rx sys msg[~p]~n", [Msg]);
	not_supported;


handle({hwswitch, _From, notif, {torrent, _, Msg} }) ->
	maybe_stop_torrent(Msg);

handle({hwswitch, _From, notif, _}) ->
	not_supported;

handle(Other) ->
	log(warning, "stopper: Unexpected message: ", [Other]).



handle_api_error(Reason) ->
	log(error, "cannot stop torrent, reason:", [Reason]).


handle_api_response(RequestId, Result) ->
	ok.



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

maybe_stop(true, {torrent, _Priority, {Name, Id, ?STATE_COMPLETED, DL}}) ->
	?CLIENT:request(undefined, SessionId, torrent.remove, Id, []);


maybe_stop(_, _Msg) ->
	no_autostop.
	

%% ----------------------          ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  HELPERS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------          ------------------------------
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

%clog(Ctx, Sev, Msg, Ps) ->
%	?SWITCH:publish(log, {Ctx, {Sev, Msg, Ps}}).

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

