%% Author: Jean-Lou Dupont
%% Created: 2009-08-03
%% Description: 
-module(transmission_tools).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([
		 to_string/1,
		 cond_add_to_list/3
		,format_encoded_list/1
		 ]).


%%
%% API Functions
%%

to_string(StringTerm) ->
	Binary=erlang:iolist_to_binary(StringTerm),
	erlang:binary_to_list(Binary).
	
	

format_encoded_list("")    -> "";
format_encoded_list(Liste) -> "?"++Liste.




%% Conditionally add a tuple to a list
%% Value = list() | atom() | string()
cond_add_to_list(Key, Value, List) when length(Value) > 0 ->
	List++[{Key, Value}];

cond_add_to_list(_Key, _Value, List) ->
	List.



