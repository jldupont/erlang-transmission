%% Author: Jean-Lou Dupont
%% Created: 2009-08-17
%% Description: TODO: Add description to test
-module(test).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([
		 start/0,
		 stop/0,
		 loop/0
		 ]).

-export([
		 t1/0
		 ]).

%%
%% API Functions
%%
start() ->
	Pid=spawn_link(?MODULE, loop, []),
	register(?MODULE, Pid),
	transmission_client:start_link(),
	Pid ! start,
	{ok, Pid}.

stop() ->
	?MODULE ! stop.



%%
%% Local Functions
%%
loop() ->
	receive
		
		stop ->
			exit(ok);
		
		start ->
			Rd={self(), reply},
			transmission_client:req(Rd, "", session.get, [], []);

		{reply, {session_id, {_, Sid} } } ->
			put(sid, Sid),
			Rd={self(), reply},
			transmission_client:req(Rd, Sid, torrent.get, [], []);
		
		{reply, {response, Code, Headers, Body}} ->
			List=erlang:binary_to_list(Body),
			{ok, Parsed}=json:decode_string(List),
			io:format("Parsed [~p]~n", [Parsed]);
			%%io:format("Body [~p]~n", [Body]);

		Other ->
			io:format("Other [~p]~n", [Other])
	
	end,
	loop().


t1() ->
	%%Params=[["arguments",[["fields", ["id", "name"]]]], "method=torrent-get", "tag=12345"],
	%%Params=[{string, "arguments"},{string, "details"}],
	%%Params=[{string, "arguments"},{string, "details"}],
	Params={obj, [{"arguments",  {obj, [{"fields", "id"}]} 
				   }]},
	Ret=ktuo_json:encode(Params),
	F=lists:flatten(Ret),
	io:format("~s~n",[F]).
