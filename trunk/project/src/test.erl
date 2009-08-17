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
			put(rd, Rd),
			transmission_client:req(Rd, "", session.get, [], []);

		{reply, {session_id, {_, Sid} } } ->
			put(sid, Sid),
			Rd=get(rd),
			transmission_client:req(Rd, Sid, session.get, [], []);
		
		{reply, Response} ->
			io:format("Response [~p]~n", [Response]);

		Other ->
			io:format("Other [~p]~n", [Other])
	
	end,
	loop().
