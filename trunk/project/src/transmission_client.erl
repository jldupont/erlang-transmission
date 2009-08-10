%% Author: Jean-Lou Dupont
%% Created: 2009-08-10
%% Description: 
%% TODO: configurable port
%%
-module(transmission_client).

%%
%% Defines
%%
-define(API, "http://localhost:9091/transmission/rpc").

%%
%% Exported Functions
%%
-export([
		 do_request/4,
		 do_request/5,
		 do_request/7
		 ]).
 
-export([
		 reply/2
		 ]).

%%
%% API Functions
%%



%%
%% Local Functions
%%

%% @private
reply(undefined, Message) ->
	io:format("~p:reply(~p)~n", [?MODULE, Message]),
	Message;

%% @private
reply({From, Context}, Message) ->
	From ! {Context, Message}.




%% @private
do_request(ReturnDetails, Timeout, Req, Headers) ->
	do_request(get, ReturnDetails, Timeout, Req, Headers).

%% @private
do_request(Type, ReturnDetails, Timeout, Req, Headers) ->
	CompleteReq=?API++Req,
	Ret = http:request(Type, {CompleteReq, Headers}, [{timeout, Timeout}], [{sync, false}]),
	case Ret of

		%% Response will be messaged
		{ok, RequestId} ->
			put({requestid, RequestId}, ReturnDetails);
	
		{error, Reason} ->
			reply(ReturnDetails, {request_error, Reason})
	end.

%% @private
do_request(Type, ReturnDetails, Timeout, Req, Headers, ContentType, Body) ->
	CompleteReq=?API++Req,
	Ret = http:request(Type, {CompleteReq, Headers, ContentType, Body}, [{timeout, Timeout}], [{sync, false}]),
	case Ret of

		{ok, RequestId} ->
			put({requestid, RequestId}, ReturnDetails);
	
		{error, Reason} ->
			reply(ReturnDetails, {request_error, Reason})
	end.

