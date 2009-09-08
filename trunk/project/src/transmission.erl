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
-define(SERVER, transmission).
-define(SWITCH, transmission_hwswitch).
-define(BUSSES, []).
-define(RPCTIMEOUT, 2000).


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
	   
		,daemon_api/1
		,req/4
		 ]).

-export([
		 loop/1, 
		 rpc/1,
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



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Local Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


rpc(Q) ->
	?SERVER ! {self(), Q},
	receive
		{?SERVER, Reply} ->
			Reply;
	
		Other ->
			clog(daemon_api, error, "~p rpc: received [~p]~n", [[?MODULE, Other]]),
			{error, rpcerror}
	
	after ?RPCTIMEOUT ->
			
			%io:format("~p: rpc timeout~n",[?MODULE]),
			{error, rpcerror}
	end.




%% @private
%% 
%% Server loop
%%
loop(Args) ->
	receive
		
		stop ->
			exit(ok);
	
		{request, ReplyDetails, Method, MandatoryParams, OptionalParams} ->
			doreq(ReplyDetails, Method, MandatoryParams, OptionalParams)
	
	end,
	loop(Args).



%% @private
reply(undefined, Message) ->
	io:format("~p:reply(~p)~n", [?MODULE, Message]),
	Message;

%% @private
reply({From, Context}, Message) ->
	From ! {Context, Message}.




doreq(ReplyDetails, Method, MandatoryParams, OptionalParams) ->
	reply(ReplyDetails, request_done).



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
	[].

