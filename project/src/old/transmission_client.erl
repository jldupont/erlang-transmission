%% Author: Jean-Lou Dupont
%% Created: 2009-08-10
%% Description: 
%% TODO: configurable port
%%
%% status=4 -> "downloading"
%% status=8 -> "seeding"  aka "completed"
%%
%% NOTES
%%  - do not use "localhost": module HTTP barks 
%%
-module(transmission_client).

%%
%% Defines
%%
-define(API, "http://127.0.0.1:9091/transmission/rpc").
-define(TIMEOUT, 2000).
-define(TOOLS, mswitch_tools).


%%
%% Exported Functions
%%
-export([
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
		 loop/0
		 ]).

%%
%% API Functions
%%
%% @spec start() -> {ok, Pid}
start_link() ->
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
			hresponse(RequestId, Result)
		
	end,
	loop().



%% ----------------------                   ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  BT Transmission  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%       API         %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------                   ------------------------------




%% ADD REQUESTS BELOW
%% ^^^^^^^^^^^^^^^^^^


%% @spec request(ReturnDetails, session.get, _MandatoryParams, _OptionalParams) -> {session.id, SessionId} | {error, Reason}

%%%%%%%%%%%%% session.get

request(Rd, SessionId, session.get, [], []) ->
	doreq(Rd, SessionId, get, "session-get", [], []);

request(ReturnDetails, _SessionId, session.get, M, O) ->
	reply(ReturnDetails, {error, {unsupported_params, M, O}});


%%%%%%%%%%%%% session.stats

request(Rd, SessionId, session.stats, [], []) ->
	doreq(Rd, SessionId, get, "session-stats", [], []);



%%%%%%%%%%%%% Remove 1 torrent

request(Rd, SessionId, torrent.remove, Id, []) ->
	doreq(Rd, SessionId, get, "torrent-remove", [{"ids", Id}], []);


%%%%%%%%%%%%% Retrieves some parameters 
%%%%%%%%%%%%% associated with *all* active torrents

request(Rd, SessionId, torrent.get, [], []) ->
	doreq(Rd, SessionId, post, "torrent-get", 
		"{"++
			  "\"arguments\":"++
			  		"{\"fields\":[\"id\", \"name\", \"status\",\"files\", \"downloadDir\"]},"++
			  "\"method\":\"torrent-get\""++
		"}"	);



%% @private
%% >>>>>>>>>>>>>>  catch-all <<<<<<<<<<<<<<<<<<
request(ReturnDetails, _SessionId, Method, _MandatoryParams, _OptionalParams) ->
	reply(ReturnDetails, {error, {unsupported_method, Method}}).




%% ----------------------              ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  REQUEST     %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%  GENERATORS  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------              ------------------------------


%% @private
doreq(Rd, SessionId, get, Method, MandatoryParams, OpParams) ->
	Params=[{"method", Method}]++lists:append(MandatoryParams, OpParams),
	PL=?TOOLS:encode_list(Params),
	Req=format_encoded_list(PL),
	Headers=cond_add_to_list("X-Transmission-Session-Id", SessionId, []),	
	do_request(get, Rd, ?TIMEOUT, Method, Req, Headers).


doreq(Rd, SessionId, post, Method, Body) ->
	Headers=cond_add_to_list("X-Transmission-Session-Id", SessionId, []),	
	do_request(post, Rd, ?TIMEOUT, Method, Headers, "application/json", Body).



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
do_request(Type, ReturnDetails, Timeout, Method, Headers, ContentType, Body) ->
	Ret = http:request(Type, {?API, Headers, ContentType, Body}, [{timeout, Timeout}], [{sync, false}]),
	case Ret of

		{ok, RequestId} ->
			put({requestid, RequestId}, ReturnDetails),
			put({method, RequestId}, Method);
	
		{error, Reason} ->
			reply(ReturnDetails, {request_error, Reason})
	end.




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



%% ----------------------           ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  HELPERS  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------           ------------------------------


%% @private
reply(undefined, Message) ->
	io:format("~p:reply(~p)~n", [?MODULE, Message]),
	Message;

%% @private
reply({From, Context}, Message) ->
	From ! {Context, Message}.




format_encoded_list("")    -> "";
format_encoded_list(Liste) -> "?"++Liste.


%% Conditionally add a tuple to a list
%% Value = list() | atom() | string()
cond_add_to_list(Key, Value, List) when length(Value) > 0 ->
	List++[{Key, Value}];

cond_add_to_list(_Key, _Value, List) ->
	List.
