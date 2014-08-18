%% common definitions:

-define(COMMON_DEFS, true).

-define(COMMANDS_CONFIG_KEY, commands).
-define(COMMANDS_DATA_SOURCE, data_source).
-define(GLOBAL_HANDLER_CONFIG_KEY, global_handler).

-record(global_config, {main_config_dir = "." :: string(),
                        commands = []:: [{CommandName :: atom(), CommandModule :: atom()}],
                        global_handler = undefined :: 'undefined' | atom() | {atom(), atom()},
                        other = [] :: [{Key :: atom(), Value :: term()}]}).

-record(login_info, {login_name = "" :: string(), is_admin = false :: boolean()}).

-record(execution_state, {global_handler = undefined :: 'undefined' | atom() | {atom(), atom()},
                          device_name = "" :: string(),
                          session = undefined :: 'undefined' | pid(),
                          login_info = undefined :: 'undefined' | #login_info{},
                          commands_info = [] :: [{CommandName :: atom(), CommandBody :: [string()], CommandHelp :: string()}],
                          current_cli_mode = undefined :: 'undefined' | atom()}).

%% parser definitions:

-record(ambiguous_parse_result, {}).

-record(incomplete_parse_result, {}).

-record(unsuccessful_parse_result, {}).

-record(successful_parse_result, {command = undefined :: 'undefined' | {Name :: atom(), Module :: atom()}, can_continue = false :: boolean()}).

-record(command_entry, {module = undefined :: 'undefined' | atom(), command_line_rest = "" :: string()}).

%% client handler definitions:

-record(client_handler_state, {config :: #global_config{},
                               execution_state :: #execution_state{},
                               command_chain = [] :: [#command_entry{}],
                               endpoint = undefined :: 'undefined' | pid(),
                               extension_generator = undefined :: 'undefined' | fun((string()) -> [string()])}).