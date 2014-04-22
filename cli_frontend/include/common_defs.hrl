%% common definitions:

-define(COMMANDS_CONFIG_KEY, commands).
-define(COMMANDS_DATA_SOURCE, data_source).
-define(GLOBAL_HANDLER_CONFIG_KEY, global_handler).

-record(global_config, {main_config_dir = "." :: string(),
                        commands = []:: [{CommandName :: atom(), CommandModule :: atom()}],
                        global_handler = undefined :: 'undefined' | atom() | {atom(), atom()},
                        other = [] :: [{Key :: atom(), Value :: term()}]}).

-record(execution_state, {session = undefined :: 'undefined' | pid(),
                          commands_info = [] :: [{CommandName :: atom(), CommandBody :: [string()], CommandHelp :: string()}],
                          current_cli_mode :: atom()}).

-record(ambiguous_parse_result, {}).

-record(incomplete_parse_result, {}).

-record(unsuccessful_parse_result, {}).

-record(successful_parse_result, {command = undefined :: 'undefined' | {Name :: atom(), Module :: atom()}, can_continue = false :: boolean()}).