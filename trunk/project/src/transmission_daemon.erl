%% Author: Jean-Lou Dupont
%% Created: 2009-08-07
%% Description: TODO: Add description to transmission
-module(transmission_daemon).

%%
%% Defines
%%
-define(SERVER, transmission).

%%
%% Exported Functions
%%
-export([
		 start/0,
		 start/1,
		 stop/0,
		 
		 %% API
		 req/4
		 ]).

-export([
		 dostart/1,
		 loop/1, 
		 rpc/1,
		 reply/2,
		 doreq/4
		 ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Exported Functions - not really part of API per-se
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start() ->
	dostart([start]).

start(Args) ->
	dostart(Args).

stop() ->
	?SERVER ! stop.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% RPC Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
req(ReplyDetails, Method, MandatoryParams, OptionalParams) ->
	rpc({request, ReplyDetails, Method, MandatoryParams, OptionalParams}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Local Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


rpc(Q) ->
	?SERVER ! {self(), Q},
	receive
		{?SERVER, Reply} ->
			Reply;
	
		Other ->
			error_logger:error_msg("~p rpc: received [~p]~n", [?MODULE, Other]),
			rpcerror
	
	after 2000 ->
			
			io:format("~p: rpc timeout~n",[?MODULE]),
			rpcerror
	end.


dostart(Args) ->
	Pid=spawn_link(?MODULE, loop, Args),
	register(?SERVER, Pid),
	{ok, Pid}.




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
	ok.