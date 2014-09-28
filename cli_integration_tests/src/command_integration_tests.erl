-module(command_integration_tests).

-include_lib("eunit/include/eunit.hrl").

-include("integration_tests_defs.hrl").

-define(COMMAND_UNAUTH, "Can't execute command for unauthenticated user").
-define(UNKNOWN_PONG_COMMAND, "Command's execution is failed due to the following: {parser_fail,[112,111,110,103],unknown_command}").

integration_test_() ->
    integration_tests_common:create_tests_entry([
        {"command: execute command by unauthenticated user",
         ["ping 192.168.0.1"],
         ["@CliDemo>ping 192.168.0.1", ?COMMAND_UNAUTH, ?COMMAND_FAIL, "@CliDemo>"]},
        {"command: execute usual command by user",
         ["login", "guest", "idclip", "ping 192.168.0.1", "logout"],
         ?GUEST_LOGIN ++ [?GREETING, "guest@CliDemo>ping 192.168.0.1"] ++ ?PING_OUTPUT ++ ?GUEST_LOGOUT},
        {"command: execute usual command by admin",
         ["login", "root", "iddqd", "ping 192.168.0.1", "logout"],
         ?ADMIN_LOGIN ++ [?GREETING, "root@CliDemo#ping 192.168.0.1"] ++ ?PING_OUTPUT ++ ?ADMIN_LOGOUT]},
        {"command: pass wrong command params by user",
         ["login", "guest", "idclip", "ping", "logout"],
         ?GUEST_LOGIN ++ [?GREETING, "guest@CliDemo>ping", ?PING_BAD_ARGS, ?COMMAND_FAIL] ++ ?GUEST_LOGOUT]},
        {["login", "root", "iddqd", "ping", "logout"],
         %"@CliDemo>login\nlogin:root\npassword:\nsome greeting message\nroot@CliDemo#ping\nCommand's execution is failed due to the following: {parser_fail,[112,105,110,103],{ping,creation_error,bad_args}}\nCommand execution failed. Return code is 255\nroot@CliDemo#logout\nYou are logged out\n@CliDemo>"
         [?LOGIN, ?GREETING, "root@CliDemo#" ++ ?PING_BAD_ARGS, ?COMMAND_FAIL, ?ADMIN_LOGOUT],
         "command: pass wrong command params by admin"},
        {["login", "guest", "idclip", "pong", "logout"],
         %"@CliDemo>login\nlogin:guest\npassword:\nsome greeting message\nguest@CliDemo>pong\nCommand's execution is failed due to the following: {parser_fail,[112,111,110,103],unknown_command}\nCommand execution failed. Return code is 255\nguest@CliDemo>logout\nYou are logged out\n@CliDemo>"
         [?LOGIN, ?GREETING, "guest@CliDemo>" ++ ?UNKNOWN_PONG_COMMAND, ?COMMAND_FAIL, ?GUEST_LOGOUT],
         "command: execute unknown command by user"},
        {["login", "root", "iddqd", "pong", "logout"],
         %"@CliDemo>login\nlogin:root\npassword:\nsome greeting message\nroot@CliDemo#pong\nCommand's execution is failed due to the following: {parser_fail,[112,111,110,103],unknown_command}\nCommand execution failed. Return code is 255\nroot@CliDemo#logout\nYou are logged out\n@CliDemo>"
         [?LOGIN, ?GREETING, "root@CliDemo#" ++ ?UNKNOWN_PONG_COMMAND, ?COMMAND_FAIL, ?ADMIN_LOGOUT],
         "command: execute unknown command by admin"},
        {["login", "guest", "idclip", "configure terminal", "logout"],
         %"@CliDemo>login\nlogin:guest\npassword:\nsome greeting message\nguest@CliDemo>configure terminal\nCommand's execution is failed due to the following: {precondition_check_fail,access_denied}\nCommand execution failed. Return code is 255\nguest@CliDemo>logout\nYou are logged out\n@CliDemo>"
         [?LOGIN, ?GREETING, "guest@CliDemo>" ++ ?ACCESS_DENIED, ?COMMAND_FAIL, ?GUEST_LOGOUT],
         "command: execute privileged command by user"},
        {["login", "root", "iddqd", "configure terminal", "logout"],
         %"@CliDemo>login\nlogin:root\npassword:\nsome greeting message\nroot@CliDemo#configure terminal\nroot@CliDemo (config)#logout\nYou are logged out\n@CliDemo>"
         [?LOGIN, ?GREETING, "root@CliDemo#root@CliDemo (config)#You are logged out."],
         "command: execute privileged command by admin"}]).