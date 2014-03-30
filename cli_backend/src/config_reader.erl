%% @author std-string

-module(config_reader).

-include("common_defs.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

-export([read/1]).

read(MainConfigFile) ->
    MainConfig = erlang_term_utils:read_from_file(MainConfigFile),
    lists:foldl(fun(ConfigItem, Config) -> process(ConfigItem, Config) end, #global_config{}, MainConfig).

%% ====================================================================
%% Internal functions
%% ====================================================================

process({?COMMANDS_CONFIG_KEY, CommandsData}, Config) ->
    CommandsSource = config_utils:get_config(CommandsData, ?COMMANDS_DATA_SOURCE, 1),
    Commands = erlang_term_utils:read_from_file(CommandsSource),
    Config#global_config{commands = Commands};
process({?CLI_FSM_CONFIG_KEY, CliFsmData}, Config) ->
    CliFsmSource = config_utils:get_config(CliFsmData, ?CLI_FSM_DATA_SOURCE, 1),
    CliFsm = erlang_term_utils:read_from_file(CliFsmSource),
    Config#global_config{cli_fsm = CliFsm};
process(Other, Config) ->
    ConfigOther = Config#global_config.other,
    Config#global_config{other = Other ++ ConfigOther}.
