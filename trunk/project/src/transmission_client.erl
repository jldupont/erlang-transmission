%% Author: Jean-Lou Dupont
%% Created: 2009-08-10
%% Description: 
%% TODO: configurable port
%%
%% status=4 -> "downloading"
%% status=8 -> "seeding"  aka "completed"
%%
%% NOTES
%%  - do not use "localhost": module HTTP barks (probably because a 'proxy' must be defined?)
%%
-module(transmission_client).

%%
%% Defines
%%
-define(API, "http://127.0.0.1:9091/transmission/rpc").
-define(TIMEOUT, 2000).
-define(TOOLS, mswitch_tools).
-define(JSON, transmission_json).


%%
%% Exported Functions
%%
-export([
		request/5 
		,decode/1
		,extract/2
		]).

 

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
%do_request(Rd, To, Method, Req) ->
%	do_request(Rd, To, Method, Req, []).

%% @private
%do_request(ReturnDetails, Timeout, Method, Req, Headers) ->
%	do_request(get, ReturnDetails, Timeout, Method, Req, Headers).

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



%% ----------------------           ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  HELPERS  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------           ------------------------------
decode(S) ->
	try
		L=erlang:binary_to_list(S),
		?JSON:decode_string(L)
	catch
		_:_ ->
			error
	end.

extract(JS, top) ->
	io:format("extracting from: ~p~n~n~n", [JS]),
	try
		{struct, Top}=JS,
		Top
	catch
		_:_ -> error
	end;


extract(JS, arguments) ->
	io:format("arguments: extracting from: ~p~n~n~n", [JS]),
	try
		{struct, Top}=JS,
		[{arguments, Arguments}] = Top,
		Arguments
	catch
		_:_ -> error
	end;


extract(JS, torrents) ->
	io:format("extracting from: ~p~n~n~n", [JS]),
	try
		{struct, [{arguments, {struct, Torrents},_,_,_}]
		}=JS,
		Torrents
	catch
		_:_ -> error
	end.




example() -> "
decoded: body<{ok,
               {struct,
                [{arguments,
                  {struct,
                   [{torrents,
                     {array,
                      [{struct,
                        [{downloadDir,\"/home/jldupont\"},
                         {files,
                          {array,
                           [{struct,
                             [{bytesCompleted,2953628},
                              {length,11784604},
                              {name,
                               \"Within Temptation - Destroyed[2008] [MP3]-RoCK&BlueLadyRG/01 Destroyed.mp3\"}]},
".


%% ----------------------           ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  HELPERS  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------           ------------------------------


%% @private
reply(undefined, Message) ->
	io:format("~p:reply(~p)~n", [?MODULE, Message]),
	Message;


%% @private
reply({From, Context}, Message) ->
	try
		From ! {Context, Message},
		ok
	catch
		_:_ -> 
			io:format("reply: error sending reply~n"),
			{error, reply}
	end.




format_encoded_list("")    -> "";
format_encoded_list(Liste) -> "?"++Liste.


%% @doc Conditionally add a tuple to a list
%% Value = list() | atom() | string()
%%
cond_add_to_list(Key, Value, List) when length(Value) > 0 ->
	List++[{Key, Value}];

cond_add_to_list(_Key, _Value, List) ->
	List.
