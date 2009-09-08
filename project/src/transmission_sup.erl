%% Author: Jean-Lou Dupont
%% Created: 2009-09-08
%% Description: TODO: Add description to transmission_sup
-module(transmission_sup).


-behavior(supervisor).


%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([
	 init/1,
	 start_link/0,
	 start_link/1
        ]).

%% ====================================================================
%% Server functions
%% ====================================================================
start_link() ->
	start_link([]).

start_link(Args) ->
	%ResultI=inets:start(),
	process_flag(trap_exit, true),
	ResultS=supervisor:start_link({local, ?MODULE}, ?MODULE, Args),
	%io:format("Results: i<~p> s<~p>~n", [ResultI, ResultS]),
	ResultS.

		

%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}
%% --------------------------------------------------------------------
init(_Args) ->

	Logger=transmission_log,
	Config=transmission_config,
	Hwswitch=transmission_hwswitch,
	Clock=transmission_clock,	
	AppCtl=transmission_appctl,
	App=transmission_daemon,
	
	%% Add all modules here
	%% List the modules that require HWSWITCH bus access
	HS_Modules = [Logger, Clock, Config, AppCtl, App 
				  ],
	
	%% List the modules that require configuration
	%%
	CF_Modules = [Logger, AppCtl, App ],
	

    Child_logger = {Logger,{Logger, start_link,[{logfilename, "/var/log/twitter.log"}]},
	      permanent,2000,worker,[Logger]},
	
    Child_switch = {Hwswitch,{Hwswitch, start_link,[mods, HS_Modules]},
	      permanent,2000,worker,[Hwswitch]},
	
    Child_clock = {Clock,{Clock, start_link,[]},
	      permanent,2000,worker,[Clock]},

	Child_appctl = {AppCtl,{AppCtl, start_link,[CF_Modules]},
	      permanent,2000,worker,[AppCtl]},

	Child_config = {Config,{Config, start_link,[CF_Modules]},
	      permanent,2000,worker,[Config]},

	Child_app = {App,{App, start_link,[]},
	      permanent,2000,worker,[App]},
	
	Children = [Child_logger, Child_switch, Child_clock, Child_appctl,  
				Child_config, Child_app],
	
	
    {ok,{{one_for_one,5,1}, Children }}.

