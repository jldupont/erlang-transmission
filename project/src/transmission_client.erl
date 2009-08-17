%% Author: Jean-Lou Dupont
%% Created: 2009-08-10
%% Description: 
%% TODO: configurable port
%%
-module(transmission_client).

%%
%% Defines
%%
-define(API, "http://127.0.0.1:9091/transmission/rpc").
-define(TIMEOUT, 2000).

%%
%% Exported Functions
%%
-export([
		start/0,
		start_link/0,
		stop/0,
		req/5
		]).

-export([
		 do_request/4,
		 do_request/5
		 ]).
 
-export([
		 reply/2,
		 loop/0,
		 test/0
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
			Method=get({method, RequestId}),
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
			hresponse(RequestId, Result)
		
	end,
	loop().


%% @spec request(ReturnDetails, session.get, _MandatoryParams, _OptionalParams) -> {session.id, SessionId} | {error, Reason}
request(Rd, SessionId, session.get, [], []) ->
	doreq(Rd, SessionId, get, "session-get", [], []);


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
doreq(Rd, SessionId, get, Method, MandatoryParams, OpParams) ->
	Params=[{"method", Method}]++lists:append(MandatoryParams, OpParams),
	PL=tools:encode_list(Params),
	Req=tools:format_encoded_list(PL),
	Headers=tools:cond_add_to_list("X-Transmission-Session-Id", SessionId, []),	
	do_request(get, Rd, ?TIMEOUT, Method, Req, Headers).





%% @private
do_request(Rd, To, Method, Req) ->
	do_request(Rd, To, Method, Req, []).

%% @private
do_request(ReturnDetails, Timeout, Method, Req, Headers) ->
	do_request(get, ReturnDetails, Timeout, Method, Req, Headers).

%% @private
%%   Method = string() % just for request context
%%
do_request(Type, ReturnDetails, Timeout, Method, Req, Headers) ->
	CompleteReq=?API++Req,
	%%io:format("req: [~p] Headers[~p]~n", [CompleteReq, Headers]),
	Ret = http:request(Type, {CompleteReq, Headers}, [{timeout, Timeout}], [{sync, false}]),
	case Ret of

		%% Response will be messaged
		{ok, RequestId} ->
			put({requestid, RequestId}, ReturnDetails),
			put({method, RequestId}, Method);
	
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


hresponse(Rid, Result) ->
	Rd=get({requestid, Rid}),
	Method=get({method, Rid}),
	erase({requestid, Rid}),
	erase({method, Rid}),
	Code=tools:extract(Result, http.code),
	Headers=tools:extract(Result, headers),
	hr(Rid, Rd, Method, Result, Code, Headers).


hr(_Rid, Rd, _Method, _Result, 409, Headers) ->
	Sid=tools:kfind("x-transmission-session-id", Headers, not_found),
	reply(Rd, {session_id, Sid});

hr(Rid, Rd, Method, Result, Code, Headers) ->
	reply(Rd, {response, Result}).




test() ->
	start(),
	req(undefined, "", session.get, [], []).
