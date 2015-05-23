%% definitions for integration tests

-record(integration_test_state, {service = undefined :: 'undefined' | port(), terminal_cmd = "" :: string()}).

-define(SERVICE_NODE, 'cli_service_node@polygon-vm').
-define(SERVICE_PROCESS, cli_terminal_listen_endpoint).
-define(SERVICE_BIN, "service_ebin").
-define(SERVICE_ARGS, " -noshell -pa ../ebin -pa ../../cli_common/ebin -pa ../../cli_command_parser/ebin -sname ~s -eval \"application:start(cli_service_application)\" >> /tmp/out").
-define(SERVICE_ENDPOINT_ADDRESS, {127, 0, 0, 1}).
-define(SERVICE_ENDPOINT_PORT, 6666).
-define(INPUT_DATA, "/tmp/input").
-define(CLI_TERMINAL_BIN, "cli_terminal_bin").
-define(CLI_TERMINAL_EXEC, "cli_terminal").
-define(CLI_TERMINAL_ARGS, " --config=cli_terminal_data/cli_terminal.conf < " ++ ?INPUT_DATA).
-define(CRASH_DUMP_FILE, "erl_crash.dump").

%% common output
-define(LOGIN(Username), ["login:" ++ Username, "password:"]).
-define(ADMIN_LOGIN, ?LOGIN("root")).
-define(GUEST_LOGIN, ?LOGIN("guest")).
-define(GREETING, "Default greeting message").

-define(LOGOUT_RESULT(Username), ["User \"" ++ Username ++ "\" is logged out"]).
-define(ADMIN_LOGOUT_RESULT, ?LOGOUT_RESULT("root")).
-define(GUEST_LOGOUT_RESULT, ?LOGOUT_RESULT("guest")).
-define(ADMIN_LOGOUT, ["root@CliDemo#logout"] ++ ?ADMIN_LOGOUT_RESULT).
-define(ADMIN_LOGOUT_SHORTCUT, ["root@CliDemo#l"] ++ ?ADMIN_LOGOUT_RESULT).
-define(GUEST_LOGOUT, ["guest@CliDemo>logout"] ++ ?GUEST_LOGOUT_RESULT).
-define(GUEST_LOGOUT_SHORTCUT, ["guest@CliDemo>l"] ++ ?GUEST_LOGOUT_RESULT).
-define(CONFIG_LOGOUT, ["root@CliDemo (config)#end", "root@CliDemo#logout"] ++ ?ADMIN_LOGOUT_RESULT).
-define(INTERFACE_LOGOUT, ["root@CliDemo (config-if)#end", "root@CliDemo#logout"] ++ ?ADMIN_LOGOUT_RESULT).
-define(IRANGE_LOGOUT, ["root@CliDemo (config-if-range)#end", "root@CliDemo#logout"] ++ ?ADMIN_LOGOUT_RESULT).
-define(VLAN_LOGOUT, ["root@CliDemo (config-vlan)#end", "root@CliDemo#logout"] ++ ?ADMIN_LOGOUT_RESULT).

-define(COMMAND_FAIL, "Command execution failed. Return code is 255").
-define(PING_OUTPUT, ["ping line 1", "ping line 2", "ping line 3"]).
-define(BAD_ARGS, "Bad arguments").
-define(ACCESS_DENIED, "Access denied").
-define(UNKNOWN_COMMAND, "Command's creation is failed due to the following reason: unknown_command").
-define(UNSUITABLE_CHAR, "Command's creation is failed due to the following reason: unsuitable_char").
-define(UNSUITABLE_COMMAND, "Unsuitable command").
-define(CONFIG_TERM, "root@CliDemo#configure terminal").
-define(CONFIG_TERM_SHORTCUT, "root@CliDemo#c t").
-define(INTERFACE, "root@CliDemo (config)#interface someinterface 0/1").
-define(INTERFACE_RANGE, "root@CliDemo (config)#interface range someinterface 0/1,3-5").
-define(VLAN, "root@CliDemo (config)#vlan 666").