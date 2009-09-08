%% Author: Jean-Lou Dupont
%% Created: 2009-09-08
%% Description: TODO: Add description to transmission_app
-module(transmission_app).


%%
%% API functions
%%
-export([
		 start/0,
		 stop/0,
		
		 %%keep compiler happy
		 loop/0
		 ]).

start() ->
	Pid=spawn(?MODULE, loop, []),
	register(?MODULE, Pid),
	Pid ! start,
	{ok, Pid}.

stop() ->
	?MODULE ! stop.


%% Just to keep things running
loop() ->
	receive
		start ->
			%% Start 'inets' before anything or else
			%% the whole system does not want to start
			inets:start(),
			transmission_sup:start_link();
		
		stop ->
			exit(normal)
	end,
	loop().

