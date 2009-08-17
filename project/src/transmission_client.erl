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
-define(TIMEOUT, 2000).

%%
%% Exported Functions
%%
-export([
		start/0,
		start_link/0,
		stop/0,
		req/4
		]).

-export([
		 do_request/4,
		 do_request/5,
		 do_request/7
		 ]).
 
-export([
		 reply/2,
		 loop/0
		 ]).

%%
%% API Functions
%%
%% @spec start() -> {ok, Pid}
start() ->
	inets:start(),
	Pid = spawn(?MODULE, loop, []),
	register(?MODULE, Pid),
	{ok,Pid}.


%% @spec start_link() -> {ok, Pid}
start_link() ->
	inets:start(),
	Pid = spawn_link(?MODULE, loop, []),
	register(?MODULE, Pid),
	{ok,Pid}.


%% @spec stop() -> ok
stop() ->
	?MODULE ! stop,
	ok.


%% 
req(ReturnDetails, SessionId, Method, MandatoryParams, OptionalParams) ->
	Ret= ?MODULE ! {request, ReturnDetails, SessionId, Method, MandatoryParams, OptionalParams},
	case Ret of
		{request, ReturnDetails, SessionId, Method, MandatoryParams, OptionalParams} ->
			ok;
		_ -> 
			error
	end.




%% @private
loop() ->
	receive
		stop ->
			exit(ok);
		
		{request, ReturnDetails, SessionId, Method, MandatoryParams, OptionalParams} ->
			request(ReturnDetails, SessionId, Method, MandatoryParams, OptionalParams);

		{http, {RequestId, {error, Reason}}} ->
			ReturnDetails=get({requestid, RequestId}),
			erase({requestid, RequestId}),
			reply(ReturnDetails, {error, Reason});

		%% Result = {{HttpVersion, HttpCode, HttpResponseCode}, [Headers], ResponseBody}
		%% HttpVersion = string()         (eg. "HTTP/1.1")
		%% HttpCode = integer()           (eg. "200")
		%% HttpResponseCode = string()    (eg. "OK")
		%% Headers = {key, value}, {key, value} ...
		%% ResponseBody = string()
		{http, {RequestId, Result}} ->
			ReturnDetails=get({requestid, RequestId}),
			erase({requestid, RequestId}),
			reply(ReturnDetails, {response, Result})
		
	end,
	loop().


%% @spec request(ReturnDetails, session.get, _MandatoryParams, _OptionalParams) -> {session.id, SessionId} | {error, Reason}
request(Rd, SessionId, session.get, [], []) ->
	doreq(Rd, ?TIMEOUT, get, "session-get", [], []);


request(ReturnDetails, _SessionId, session.get, M, O) ->
	reply(ReturnDetails, {error, {unsupported_params, M, O}});


%% @private
%% catch-all
request(ReturnDetails, _SessionId, Method, _MandatoryParams, _OptionalParams) ->
	reply(ReturnDetails, {error, {unsupported_method, Method}}).



%%
%% Local Functions
%%
%% @private
doreq(Rd, Timeout, get, Method, MandatoryParams, OpParams) ->
	Params=lists:append(MandatoryParams, OpParams),
	PL=tools:encode_list(Params),
	Req=Method++tools:format_encoded_list(PL),
	do_request(Rd, Timeout, get, Req);

%% @private
doreq(Rd, Timeout, post, Method, MandatoryParams, OpParams) ->
	Params=lists:append(MandatoryParams, OpParams),
	Body=tools:encode_list(Params),
	do_request(Rd, Timeout, post, Method, "application/x-www-form-urlencoded", Body).





%% @private
do_request(ReturnDetails, Timeout, Method, Req, Headers) ->
	do_request(get, ReturnDetails, Timeout, Method, Req, Headers).

%% @private
do_request(Type, ReturnDetails, Timeout, Method, Req, Headers) ->
	CompleteReq=?API++Req,
	Ret = http:request(Type, {CompleteReq, Headers}, [{timeout, Timeout}], [{sync, false}]),
	case Ret of

		%% Response will be messaged
		{ok, RequestId} ->
			put({requestid, RequestId, Method}, ReturnDetails);
	
		{error, Reason} ->
			reply(ReturnDetails, {request_error, Reason})
	end.

%% @private
do_request(Type, ReturnDetails, Timeout, Method, Req, Headers, ContentType, Body) ->
	CompleteReq=?API++Req,
	Ret = http:request(Type, {CompleteReq, Headers, ContentType, Body}, [{timeout, Timeout}], [{sync, false}]),
	case Ret of

		{ok, RequestId} ->
			put({requestid, RequestId, Method}, ReturnDetails);
	
		{error, Reason} ->
			reply(ReturnDetails, {request_error, Reason})
	end.




%% @private
reply(undefined, Message) ->
	io:format("~p:reply(~p)~n", [?MODULE, Message]),
	Message;

%% @private
reply({From, Context}, Message) ->
	From ! {Context, Message}.


