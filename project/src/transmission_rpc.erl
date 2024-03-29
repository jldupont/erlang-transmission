%% Author: Jean-Lou Dupont
%% Created: 2009-08-21
%% Description: 
-module(transmission_rpc).
-compile(export_all).

%%
%% MACROS
%%
-define(SUPPORTED_CMDS, [getapiversion, reload, status, getcmds]).
-define(API_VERSION, {1,0}).
-define(SERVER,     transmission).
-define(TIMEOUT,    1000).
-define(SWITCH,     transmission_hwswitch).


getcmds() ->
	?SUPPORTED_CMDS.

%% ----------------------              ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%% RPC handling %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------              ------------------------------

%% Retrieves the Pid of the daemon
%%
handle_rpc(ReplyTo, _FromNode, RC, status) ->
	rpc_reply(ReplyTo, {RC, os:getpid()});

%% Retrieves the supported commands through the RPC interface
%%
handle_rpc(ReplyTo, _FromNode, RC, getcmds) ->
	rpc_reply(ReplyTo, {RC, ?SUPPORTED_CMDS});


%% Reloads the configuration from file
%%
handle_rpc(ReplyTo, _FromNode, RC, reload) ->
	?SWITCH:publish(sys, reload),
	rpc_reply(ReplyTo, {RC, ok});


%% Retrieves the API version
%%
handle_rpc(ReplyTo, _FromNode, RC, getapiversion) ->
	rpc_reply(ReplyTo, {RC, ?API_VERSION});





%% CATCH-ALL
%%
handle_rpc(ReplyTo, _, _, _) ->
	rpc_reply(ReplyTo, {error, invalid_request}).


%% Validates if the RPC Command is supported
%%
rpc_validate_command(Command) when is_atom(Command) ->
	lists:member(Command, ?SUPPORTED_CMDS);

rpc_validate_command(_) ->
	invalid.


%% Replies to an RPC call.
%% The recipient loop of the message normally sits
%% in the code of the function 'rpc'.
%%
rpc_reply(ReplyTo, Message) ->
	ReplyTo ! {rpc_reply, Message}.

	
%% ----------------------                 ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------                 ------------------------------


%% @private
rpc(ReplyContext, Q) ->
	FromNode=node(),
	%%io:format("call: from[~p] Q[~p]~n", [FromNode, Q]),
	%%?TOOLS:msg("rpc: From[~p] Message[~p]", [FromNode, Q]),
	?SERVER ! {rpc, self(), {FromNode, ReplyContext, Q}},
	receive
		{rpc_reply, Reply} ->
			Reply;
	
		Other ->
			error_logger:error_msg("~p rpc: received [~p]~n", [?MODULE, Other]),
			{error, rpcerror}
	
	after ?TIMEOUT ->
			error_logger:error_msg("~p: rpc: timeout~n", [?MODULE]),
			{error, rpc_timeout}
	end.

