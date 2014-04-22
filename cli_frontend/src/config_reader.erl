%% @author stdstring

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

process({?COMMANDS_CONFIG_KEY, CommandsData}, Config, MainConfigDir) ->
    CommandsSource = config_utils:get_config(CommandsData, ?COMMANDS_DATA_SOURCE, 1, missing_commands_def),
    Commands = erlang_term_utils:read_from_file(filename:absname(CommandsSource, MainConfigDir)),
    Config#global_config{commands = Commands};
process({?GLOBAL_HANDLER_CONFIG_KEY, GlobalHandler}, Config, _MainConfigDir) ->
    Config#global_config{global_handler = GlobalHandler};
process(Other, Config, _MainConfigDir) ->
    ConfigOther = Config#global_config.other,
    Config#global_config{other = [Other] ++ ConfigOther}.
