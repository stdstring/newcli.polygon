%% cli termninal common definitions:

-define(LISTEN_SUPERVISOR_NAME, cli_terminal_listen_supervisor).
-define(ENDPOINT_SUPERVISOR_NAME, cli_terminal_supervisor).

-record(cli_terminal_config, {port_number = 65535 :: 1..65535,
                              max_client_count = 10 :: pos_integer()}).

%% socket = undefined :: 'undefined' | socket()
-record(cli_terminal_state, {socket = undefined :: 'undefined' | term()}).