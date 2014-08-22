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

-spec process(Entry :: {Key :: atom(), Value :: term()}, Config :: #global_config{}, MainConfigDir :: string()) -> #global_config{}.
process({?COMMANDS_CONFIG_KEY, CommandsData}, Config, MainConfigDir) ->
    CommandsSource = config_utils:get_config(CommandsData, ?COMMANDS_DATA_SOURCE, 1, missing_commands_def),
    Commands = erlang_term_utils:read_from_file(filename:absname(CommandsSource, MainConfigDir)),
    Config#global_config{commands = Commands};
process({?GLOBAL_HANDLER_CONFIG_KEY, GlobalHandler}, Config, _MainConfigDir) ->
    Config#global_config{global_handler = GlobalHandler};
process({?CLI_TERMINAL_CONFIG_KEY, CliTerminalConfig}, Config, _MainConfigDir) ->
    PortNumber = config_utils:get_config(CliTerminalConfig, ?PORT_NUMBER_KEY, 1, missing_port_number),
    MaxClientCount = config_utils:get_config_with_default(CliTerminalConfig, ?MAX_CLIENT_COUNT_KEY, 1, 10),
    Config#global_config{cli_terminal = #cli_terminal_config{port_number = PortNumber, max_client_count = MaxClientCount}};
process(Other, Config, _MainConfigDir) ->
    ConfigOther = Config#global_config.other,
    Config#global_config{other = [Other] ++ ConfigOther}.
