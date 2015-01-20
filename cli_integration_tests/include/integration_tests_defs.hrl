%% definitions for integration tests

-record(integration_test_state, {service = undefined :: 'undefined' | port(), terminal_cmd = "" :: string()}).

-define(SERVICE_NODE, 'cli_service_node@polygon-vm').
-define(SERVICE_PROCESS, cli_terminal_listen_endpoint).
-define(SERVICE_ARGS, " -noshell -pa ../ebin -pa ../../cli_common/ebin -pa ../../cli_command_parser/ebin -sname ~s -eval \"application:start(cli_service_application)\" >> /tmp/out").
-define(SERVICE_ENDPOINT_ADDRESS, {127, 0, 0, 1}).
-define(SERVICE_ENDPOINT_PORT, 6666).

%% common output
%%-define(ADMIN_LOGIN, ["@CliDemo>login", "login:root", "password:"]).
%%-define(GUEST_LOGIN, ["@CliDemo>login", "login:guest", "password:"]).
-define(LOGIN(Username), ["@CliDemo>login", "login:" ++ Username, "password:"]).
%%-define(GREETING, "some greeting message").
-define(GREETING, "Default greeting message").
%%-define(LOGOUT_RESULT, ["You are logged out", "@CliDemo>"]).
-define(ADMIN_LOGOUT, ["root@CliDemo#logout", "User \"root\" is logged out", "@CliDemo>"]).
-define(GUEST_LOGOUT, ["guest@CliDemo>logout", "User \"guest\" is logged out", "@CliDemo>"]).
-define(COMMAND_FAIL, "Command execution failed. Return code is 255").