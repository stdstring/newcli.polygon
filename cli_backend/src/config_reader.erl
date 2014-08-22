%% @author std-string

-module(config_reader).

-include("common_defs.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

-export([read/1]).

-spec read(MainConfigFile :: string()) -> #global_config{}.
read(MainConfigFile) ->
    MainConfigDir = filename:dirname(MainConfigFile),
    MainConfig = erlang_term_utils:read_from_file(MainConfigFile),
    Acc0 = #global_config{main_config_dir = MainConfigDir},
    lists:foldl(fun(ConfigItem, Config) -> process(ConfigItem, Config, MainConfigDir) end, Acc0, MainConfig).

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec process(Entry :: {Key :: atom(), Value :: term()}, Config :: #global_config{}, MainConfigDir :: string()) -> #global_config{}.
process({?COMMANDS_CONFIG_KEY, CommandsData}, Config, MainConfigDir) ->
    CommandsSource = config_utils:get_config(CommandsData, ?COMMANDS_DATA_SOURCE, 1, missing_commands_def),
    Commands = erlang_term_utils:read_from_file(filename:absname(CommandsSource, MainConfigDir)),
    Config#global_config{commands = Commands};
process({?CLI_FSM_CONFIG_KEY, CliFsmData}, Config, MainConfigDir) ->
    CliFsmSource = config_utils:get_config(CliFsmData, ?CLI_FSM_DATA_SOURCE, 1, missing_cli_fsm_def),
    CliFsm = erlang_term_utils:read_from_file(filename:absname(CliFsmSource, MainConfigDir)),
    Config#global_config{cli_fsm = CliFsm};
process(Other, Config, _MainConfigDir) ->
    ConfigOther = Config#global_config.other,
    Config#global_config{other = [Other] ++ ConfigOther}.
