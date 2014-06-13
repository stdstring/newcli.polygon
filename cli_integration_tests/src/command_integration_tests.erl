-module(command_integration_tests).

-include_lib("eunit/include/eunit.hrl").

-include("integration_tests_defs.hrl").

-define(LOGIN, "@CliDemo>login:password:").
-define(GREETING, "some greeting message").
-define(GUEST_LOGOUT, "guest@CliDemo>You are logged out.").
-define(ADMIN_LOGOUT, "root@CliDemo#You are logged out.").

integration_test_() ->
    integration_tests_common:create_tests_entry([
        {["ping 192.168.0.1"], ["@CliDemo>Can't execute command for unauthenticated user.", "Command execution failed. Return code is 255"], "command: execute command by unauthenticated user"},
        {["login", "guest", "idclip", "ping 192.168.0.1", "logout"], [?LOGIN, ?GREETING, "guest@CliDemo>ping line 1", "ping line 2", "ping line 3", ?GUEST_LOGOUT], "command: execute usual command by user"},
        {["login", "root", "iddqd", "ping 192.168.0.1", "logout"], [?LOGIN, ?GREETING, "root@CliDemo#ping line 1", "ping line 2", "ping line 3", ?ADMIN_LOGOUT], "command: execute usual command by admin"},
        {["login", "guest", "idclip", "ping", "logout"], [?LOGIN, ?GREETING, "guest@CliDemo>Command's execution is failed due to the following: {parser_fail,[112,105,110,103],{ping,creation_error,bad_args}}", "Command execution failed. Return code is 255", ?GUEST_LOGOUT], "command: pass wrong command params by user"},
        {["login", "root", "iddqd", "ping", "logout"], [?LOGIN, ?GREETING, "root@CliDemo#Command's execution is failed due to the following: {parser_fail,[112,105,110,103],{ping,creation_error,bad_args}}", "Command execution failed. Return code is 255", ?ADMIN_LOGOUT], "command: pass wrong command params by admin"},
        {["login", "guest", "idclip", "configure terminal", "logout"], [?LOGIN, ?GREETING, "guest@CliDemo>Command's execution is failed due to the following: {precondition_check_fail,access_denied}", "Command execution failed. Return code is 255", ?GUEST_LOGOUT], "command: execute privileged command by user"},
        {["login", "root", "iddqd", "configure terminal", "logout"], [?LOGIN, ?GREETING, "root@CliDemo#root@CliDemo (config)#You are logged out."], "command: execute privileged command by admin"}]).