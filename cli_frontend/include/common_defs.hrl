%% cli_terminal definitions

-define(LISTEN_SUPERVISOR_NAME, cli_terminal_listen_supervisor).
-define(ENDPOINT_SUPERVISOR_NAME, cli_terminal_supervisor).
-define(LISTEN_ENDPOINT_NAME, cli_terminal_listen_endpoint).

-define(CLI_TERMINAL_CONFIG_KEY, cli_terminal).
-define(PORT_NUMBER_KEY, port_number).
-define(MAX_CLIENT_COUNT_KEY, max_client_count).

-record(cli_terminal_config, {port_number = 65535 :: 1..65535,
                              max_client_count = 10 :: pos_integer()}).

%% socket = undefined :: 'undefined' | socket()
-record(cli_terminal_state, {socket = undefined :: 'undefined' | term(),
                             client_handler = undefined :: 'undefined'  | pid()}).

%% common definitions:

-define(COMMANDS_CONFIG_KEY, commands).
-define(COMMANDS_DATA_SOURCE, data_source).
-define(GLOBAL_HANDLER_CONFIG_KEY, global_handler).

-record(global_config, {main_config_dir = "." :: string(),
                        commands = []:: [{CommandName :: atom(), CommandModule :: atom()}],
                        global_handler = undefined :: 'undefined' | atom() | {atom(), atom()},
                        cli_terminal = #cli_terminal_config{} :: #cli_terminal_config{},
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
                               current_command = undefined :: 'undefined' | pid(),
                               endpoint = undefined :: 'undefined' | pid(),
                               extension_generator = undefined :: 'undefined' | fun((string()) -> [string()])}).