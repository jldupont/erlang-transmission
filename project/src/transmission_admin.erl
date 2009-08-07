%% Author: Jean-Lou Dupont
%% Created: 2009-08-07
%% Description: TODO: Add description to transmission_admin
-module(transmission_admin).


%%
%% Defines
%%
-define(DAEMON, transmission).
-define(DAEMON_MODULE, transmission_daemon).
-define(RPC_TIMEOUT, 4000).

%%
%% Exported Functions
%%
%%
%% Exported Functions
%%
-export([
		 daemon/1
		 ]).


%%
%% API Functions
%%

%% 
daemon(stop) ->
	prerpc(stop);
	
daemon(status) ->
	prerpc(status);

daemon(Other) ->
	{command_invalid, Other}.	
	

%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS
%%%%%%%%%%%%%%%%%%


prerpc(Command) ->
	case dorpc(Command) of
		{badrpc, _} ->
			daemon_not_found;

		Other ->
			Other
	end.
	


dorpc(Message) ->
	Node=tools:make_node(?DAEMON),

	case rpc:call(Node, ?DAEMON_MODULE, api, [Message], ?RPC_TIMEOUT) of
		{badrpc, Reason} ->
			io:format("daemon communication error [~p]~n", [Reason]),
			rpcerror;
		
		Other ->
			io:format("~p:dorpc: received [~p]~n", [?MODULE, Other]),
			Other
	end.
