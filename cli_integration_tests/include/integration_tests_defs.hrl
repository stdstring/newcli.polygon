%% definitions for integration tests

-record(integration_test_state, {service = undefined :: 'undefined' | port(), terminal_cmd = "" :: string()}).

-define(SERVICE_NODE, 'cli_service_node@polygon-vm').
-define(SERVICE_PROCESS, cli_terminal_listen_endpoint).
-define(SERVICE_ARGS, " -noshell -pa ../ebin -pa ../../cli_common/ebin -pa ../../cli_command_parser/ebin -sname ~s -eval \"application:start(cli_service_application)\" >> /tmp/out").
-define(SERVICE_ENDPOINT_ADDRESS, {127, 0, 0, 1}).
-define(SERVICE_ENDPOINT_PORT, 6666).

%% common output
-define(LOGIN(Username), ["@CliDemo>login", "login:" ++ Username, "password:"]).
-define(ADMIN_LOGIN, ?LOGIN("root")).
-define(GUEST_LOGIN, ?LOGIN("guest")).
%%-define(GREETING, "some greeting message").
-define(GREETING, "Default greeting message").
-define(LOGOUT_RESULT(Username), ["User \"" ++ Username ++ "\" is logged out", "@CliDemo>"]).
-define(ADMIN_LOGOUT, ["root@CliDemo#logout"] ++ ?LOGOUT_RESULT("root")).
-define(GUEST_LOGOUT, ["guest@CliDemo>logout"] ++ ?LOGOUT_RESULT("guest")).
-define(COMMAND_FAIL, "Command execution failed. Return code is 255").
-define(PING_OUTPUT, ["ping line 1", "ping line 2", "ping line 3"]).
-define(BAD_ARGS, "Bad arguments").
-define(ACCESS_DENIED, "Access denied").
-define(UNKNOWN_COMMAND, "Command's creation is failed due to the following reason: command_not_found").
-define(UNSUITABLE_COMMAND, "Unsuitable command").
-define(CONFIG_TERM, "root@CliDemo#configure terminal").
-define(INTERFACE, "root@CliDemo (config)#interface someinterface 0/1").
-define(INTERFACE_RANGE, "root@CliDemo (config)#interface range someinterface 0/1,3-5").
-define(VLAN, "root@CliDemo (config)#vlan 666").