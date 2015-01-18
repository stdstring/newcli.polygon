%% definitions for integration tests

-record(integration_test_state, {service = undefined :: 'undefined' | port(), terminal_cmd = "" :: string()}).

-define(SERVICE_NODE, 'cli_service_node@polygon-vm').
-define(SERVICE_PROCESS, cli_terminal_listen_endpoint).
-define(SERVICE_ARGS, " -noshell -pa ../ebin -pa ../../cli_common/ebin -pa ../../cli_command_parser/ebin -sname ~s -eval \"application:start(cli_service_application)\"").
-define(SERVICE_ENDPOINT_ADDRESS, {127, 0, 0, 1}).
-define(SERVICE_ENDPOINT_PORT, 6666).