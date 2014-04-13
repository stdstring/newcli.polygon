%% common definitions:

-define(COMMANDS_CONFIG_KEY, commands).
-define(COMMANDS_DATA_SOURCE, data_source).
-define(CLI_FSM_CONFIG_KEY, cli_fsm).
-define(CLI_FSM_DATA_SOURCE, data_source).

-record(user, {uid = -1 :: integer(), username = "" :: string(), access_level = 0 :: integer()}).

-record(global_config, {main_config_dir = "." :: string(),
                        commands = [] :: [{CommandName :: atom(), CommandModule :: atom()}],
                        cli_fsm = [] :: [{Key :: atom(), Value :: term()}],
                        other = [] :: [{Key :: atom(), Value :: term()}]}).

-record(cli_fsm_state_info, {current_state = undefined :: atom(), commands = [] :: [CommandName :: atom()], is_terminal = false :: boolean()}).

-record(client_config, {user = undefined :: 'undefined' | #user{},
                        cli_fsm = undefined :: 'undefined' | pid(),
                        output = undefined :: 'undefined' | pid()}).

-record(parse_result, {state = undefined :: 'undefined' | atom(),
                       command = undefined :: 'undefined' | {Name :: atom(), Module :: atom()},
                       can_continue = false :: boolean()}).
