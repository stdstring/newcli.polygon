%% definitions for integration tests

-define(INPUT_DATA, "/tmp/input").

-record(integration_test_state, {backend = undefined :: 'undefined' | port(),
                                 frontend = undefined :: 'undefined' | port(),
                                 terminal_cmd = "" :: string()}).
%% component defs
-define(BACKEND_NODE, 'backend_node@polygon-vm').
-define(BACKEND_PROCESS, global_input_endpoint).
-define(FRONTEND_NODE, 'frontend_node@polygon-vm').
-define(FRONTEND_PROCESS, cli_terminal_listen_endpoint).
-define(FRONTEND_ENDPOINT_ADDRESS, {127, 0, 0, 1}).
-define(FRONTEND_ENDPOINT_PORT, 6666).

%% common output
-define(ADMIN_LOGIN, ["@CliDemo>login", "login:root", "password:"]).
-define(GUEST_LOGIN, ["@CliDemo>login", "login:guest", "password:"]).
-define(GREETING, "some greeting message").
-define(LOGOUT_RESULT, ["You are logged out", "@CliDemo>"]).
-define(ADMIN_LOGOUT, ["root@CliDemo#logout"] ++ ?LOGOUT_RESULT).
-define(GUEST_LOGOUT, ["guest@CliDemo>logout"] ++ ?LOGOUT_RESULT).
-define(PING_OUTPUT, ["ping line 1", "ping line 2", "ping line 3"]).
-define(COMMAND_FAIL, "Command execution failed. Return code is 255").
-define(PING_BAD_ARGS, "Command's execution is failed due to the following: {parser_fail,[112,105,110,103],{ping,creation_error,bad_args}}").
-define(ACCESS_DENIED, "Command's execution is failed due to the following: {precondition_check_fail,access_denied}").
-define(UNSUITABLE_COMMAND, "Command's execution is failed due to the following: {precondition_check_fail,unsuitable_command}").
-define(CONFIG_TERM, "root@CliDemo#configure terminal").
-define(INTERFACE, "root@CliDemo (config)#interface someinterface 0/1").
-define(INTERFACE_RANGE, "root@CliDemo (config)#interface range someinterface 0/1,3-5").
-define(VLAN, "root@CliDemo (config)#vlan 666").