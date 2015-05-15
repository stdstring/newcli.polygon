%% @author stdstring

-module(command_search).

-include("authentication_defs.hrl").
-include("common_defs.hrl").

-export([search_by_name/2]).

%% ====================================================================
%% API functions
%% ====================================================================

%% TODO (std_string) : think about usefulness of this method/module
-spec search_by_name(CommandName :: atom(), Config :: #global_config{}) -> atom() | no_return().
search_by_name(CommandName, Config) ->
    Commands = Config#global_config.commands,
    list_utils:get_value_by_key(Commands, CommandName, 1, unknown_command).

%% ====================================================================
%% Internal functions
%% ====================================================================
