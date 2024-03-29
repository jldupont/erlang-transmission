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
	App=transmission,
	Notifier=transmission_notifier,
	Stopper=transmission_stopper,
	
	%% Add all modules here
	%% List the modules that require HWSWITCH bus access
	HS_Modules = [Logger, Clock, Config, AppCtl, App, Notifier, Stopper
				  ],
	
	%% List the modules that require configuration
	%%
	CF_Modules = [Logger, AppCtl, App, Notifier, Stopper ],
	

    Child_logger = {Logger,{Logger, start_link,[{logfilename, "/var/log/transmission.log"}]},
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

	Child_notif = {Notifier,{Notifier, start_link,[]},
	      permanent,2000,worker,[Notifier]},

	Child_stopper = {Stopper,{Stopper, start_link,[]},
	      permanent,2000,worker,[Stopper]},
	
	Children = [Child_logger, Child_switch, Child_clock, Child_appctl,  
				Child_config, Child_app, Child_notif, Child_stopper ],
	
	
    {ok,{{one_for_one,5,1}, Children }}.

