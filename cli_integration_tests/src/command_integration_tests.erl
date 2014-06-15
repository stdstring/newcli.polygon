-module(command_integration_tests).

-include_lib("eunit/include/eunit.hrl").

-include("integration_tests_defs.hrl").

-define(UNKNOWN_PONG_COMMAND, "Command's execution is failed due to the following: {parser_fail,[112,111,110,103],unknown_command}").

integration_test_() ->
    integration_tests_common:create_tests_entry([
        {["ping 192.168.0.1"],
         ["@CliDemo>Can't execute command for unauthenticated user.", ?COMMAND_FAIL],
         "command: execute command by unauthenticated user"},
        {["login", "guest", "idclip", "ping 192.168.0.1", "logout"],
         [?LOGIN, ?GREETING, "guest@CliDemo>ping line 1", "ping line 2", "ping line 3", ?GUEST_LOGOUT],
         "command: execute usual command by user"},
        {["login", "root", "iddqd", "ping 192.168.0.1", "logout"],
         [?LOGIN, ?GREETING, "root@CliDemo#ping line 1", "ping line 2", "ping line 3", ?ADMIN_LOGOUT],
         "command: execute usual command by admin"},
        {["login", "guest", "idclip", "ping", "logout"],
         [?LOGIN, ?GREETING, "guest@CliDemo>" ++ ?PING_BAD_ARGS, ?COMMAND_FAIL, ?GUEST_LOGOUT],
         "command: pass wrong command params by user"},
        {["login", "root", "iddqd", "ping", "logout"],
         [?LOGIN, ?GREETING, "root@CliDemo#" ++ ?PING_BAD_ARGS, ?COMMAND_FAIL, ?ADMIN_LOGOUT],
         "command: pass wrong command params by admin"},
        {["login", "guest", "idclip", "pong", "logout"],
         [?LOGIN, ?GREETING, "guest@CliDemo>" ++ ?UNKNOWN_PONG_COMMAND, ?COMMAND_FAIL, ?GUEST_LOGOUT],
         "command: execute unknown command by user"},
        {["login", "root", "iddqd", "pong", "logout"],
         [?LOGIN, ?GREETING, "root@CliDemo#" ++ ?UNKNOWN_PONG_COMMAND, ?COMMAND_FAIL, ?ADMIN_LOGOUT],
         "command: execute unknown command by admin"},
        {["login", "guest", "idclip", "configure terminal", "logout"],
         [?LOGIN, ?GREETING, "guest@CliDemo>" ++ ?ACCESS_DENIED, ?COMMAND_FAIL, ?GUEST_LOGOUT],
         "command: execute privileged command by user"},
        {["login", "root", "iddqd", "configure terminal", "logout"],
         [?LOGIN, ?GREETING, "root@CliDemo#root@CliDemo (config)#You are logged out."],
         "command: execute privileged command by admin"}]).