%% common definitions:

-record(user, {uid = -1 :: integer(), username = "" :: string(), access_level = 0 :: integer()}).

-define(COMMANDS_CONFIG_KEY, commands).
-define(COMMANDS_DATA_SOURCE, data_source).
-define(CLI_FSM_CONFIG_KEY, cli_fsm).
-define(CLI_FSM_DATA_SOURCE, data_source).

-record(config, {commands = [] :: [{CommandName :: atom(), CommandModule :: atom()}],
                 cli_fsm = [] :: [{Key :: atom(), Value :: term()}],
                 other = [] :: [{Key :: atom(), Value :: term()}]}).

-record(command_parse_result, {command_chain = [] :: [{ModuleName :: atom, CommandPid :: pid}], endpoint = undefined :: pid() | 'undefined'}).