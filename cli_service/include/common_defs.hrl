%% common definitions

-define(LISTEN_SUPERVISOR_NAME, cli_terminal_listen_supervisor).
-define(LISTEN_ENDPOINT_NAME, cli_terminal_listen_endpoint).
-define(ENDPOINT_SUPERVISOR_NAME, cli_terminal_supervisor).

-record(cli_terminal_config, {port_number = 1 :: 1..65535,
                              downtime = 1 :: pos_integer()}).
-record(global_config, {device_name = "CliDemo" :: string(),
                        cli_terminal = #cli_terminal_config{} :: #cli_terminal_config{},
                        commands = [] :: [{CommandName :: atom(), CommandModule :: atom()}],
                        cli_fsm = [] :: [{Key :: atom(), Value :: term()}],
                        other = [] :: [{Key :: atom(), Value :: term()}]}).

-record(cli_terminal_state, {socket = undefined :: 'undefined' | term(),
                             client_handler = undefined :: 'undefined' | pid()}).
-record(client_handler_state, {config :: #global_config{},
                               endpoint = undefined :: 'undefined' | pid(),
                               timer_ref = undefined :: 'undefined' | reference(),
                               cli_fsm = undefined :: 'undefined' | pid(),
                               command_module = test_command :: atom(),
                               user = undefined :: 'undefined' | #user{},
                               current_command = undefined :: 'undefined' | pid()}).