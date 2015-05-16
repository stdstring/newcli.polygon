%% @author std-string

-module(config_reader).

-include("authentication_defs.hrl").
-include("common_defs.hrl").
-include("config_defs.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

-export([read/1]).

-spec read(MainConfigFile :: string()) -> #global_config{}.
read(MainConfigFile) ->
    MainConfigDir = filename:dirname(MainConfigFile),
    MainConfig = erlang_term_utils:read_from_file(MainConfigFile),
    Acc0 = #global_config{},
    lists:foldl(fun(ConfigItem, Config) -> process(ConfigItem, Config, MainConfigDir) end, Acc0, MainConfig).

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec process(Entry :: {Key :: atom(), Value :: term()}, Config :: #global_config{}, MainConfigDir :: string()) -> #global_config{}.
process({?COMMANDS_CONFIG_KEY, CommandsData}, Config, MainConfigDir) ->
    CommandsSource = list_utils:get_value_by_key(CommandsData, ?COMMANDS_DATA_SOURCE, 1, {?MODULE, missing_commands_def}),
    Commands = erlang_term_utils:read_from_file(filename:absname(CommandsSource, MainConfigDir)),
    Config#global_config{commands = Commands};
process({?CLI_FSM_CONFIG_KEY, CliFsmData}, Config, MainConfigDir) ->
    CliFsmSource = list_utils:get_value_by_key(CliFsmData, ?CLI_FSM_DATA_SOURCE, 1, {?MODULE, missing_fsm_def}),
    CliFsm = erlang_term_utils:read_from_file(filename:absname(CliFsmSource, MainConfigDir)),
    Config#global_config{cli_fsm = CliFsm};
process({?TERMINAL_CONFIG_KEY, TerminalData}, Config, _MainConfigDir) ->
    PortNumber = list_utils:get_value_by_key(TerminalData, ?TERMINAL_PORT_NUMBER, 1, {?MODULE, missing_terminal_port}),
    Downtime = list_utils:get_value_by_key_with_default(TerminalData, ?TERMINAL_DOWNTIME, 1, ?TERMINAL_DOWNTIME_DEFAULT),
    MaxLoginCount = list_utils:get_value_by_key_with_default(TerminalData, ?TERMINAL_LOGIN_COUNT, 1, ?TERMINAL_LOGIN_COUNT_DEFAULT),
    TerminalConfig = #cli_terminal_config{port_number = PortNumber, downtime = Downtime, max_login_attempt_count = MaxLoginCount},
    Config#global_config{cli_terminal = TerminalConfig};
process(Other, Config, _MainConfigDir) ->
    ConfigOther = Config#global_config.other,
    Config#global_config{other = [Other] ++ ConfigOther}.
