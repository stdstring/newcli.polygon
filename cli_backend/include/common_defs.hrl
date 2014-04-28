%% common definitions:

-define(COMMANDS_CONFIG_KEY, commands).
-define(COMMANDS_DATA_SOURCE, data_source).
-define(CLI_FSM_CONFIG_KEY, cli_fsm).
-define(CLI_FSM_DATA_SOURCE, data_source).
-define(MIN_ACCESS_LEVEL, 0).
-define(MAX_ACCESS_LEVEL, 15).

-record(user, {uid = -1 :: integer(), username = "" :: string(), access_level = ?MIN_ACCESS_LEVEL :: integer()}).

-record(global_config, {main_config_dir = "." :: string(),
                        commands = [] :: [{CommandName :: atom(), CommandModule :: atom()}],
                        cli_fsm = [] :: [{Key :: atom(), Value :: term()}],
                        other = [] :: [{Key :: atom(), Value :: term()}]}).

-record(cli_fsm_state_info, {current_state = undefined :: atom(),
                             current_state_representation :: atom(),
                             commands = [] :: [CommandName :: atom()],
                             is_terminal = false :: boolean()}).

-record(client_config, {user = undefined :: 'undefined' | #user{},
                        cli_fsm = undefined :: 'undefined' | pid(),
                        output = undefined :: 'undefined' | pid()}).

-record(ambiguous_parse_result, {}).

-record(incomplete_parse_result, {}).

-record(unsuccessful_parse_result, {}).

-record(successful_parse_result, {command = undefined :: 'undefined' | {Name :: atom(), Module :: atom()}, can_continue = false :: boolean()}).