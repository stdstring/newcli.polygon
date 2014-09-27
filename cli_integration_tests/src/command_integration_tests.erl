-module(command_integration_tests).

-include_lib("eunit/include/eunit.hrl").

-include("integration_tests_defs.hrl").

-define(UNKNOWN_PONG_COMMAND, "Command's execution is failed due to the following: {parser_fail,[112,111,110,103],unknown_command}").

integration_test_() ->
    integration_tests_common:create_tests_entry([
        {["ping 192.168.0.1"],
         %"@CliDemo>ping 192.168.0.1\nCan't execute command for unauthenticated user\nCommand execution failed. Return code is 255\n@CliDemo>"
         ["@CliDemo>Can't execute command for unauthenticated user.", ?COMMAND_FAIL],
         "command: execute command by unauthenticated user"},
        {["login", "guest", "idclip", "ping 192.168.0.1", "logout"],
         %"@CliDemo>login\nlogin:guest\npassword:\nsome greeting message\nguest@CliDemo>ping 192.168.0.1\nping line 1\nping line 2\nping line 3\nguest@CliDemo>logout\nYou are logged out\n@CliDemo>"
         [?LOGIN, ?GREETING, "guest@CliDemo>ping line 1", "ping line 2", "ping line 3", ?GUEST_LOGOUT],
         "command: execute usual command by user"},
        {["login", "root", "iddqd", "ping 192.168.0.1", "logout"],
         %"@CliDemo>login\nlogin:root\npassword:\nsome greeting message\nroot@CliDemo#ping 192.168.0.1\nping line 1\nping line 2\nping line 3\nroot@CliDemo#logout\nYou are logged out\n@CliDemo>"
         [?LOGIN, ?GREETING, "root@CliDemo#ping line 1", "ping line 2", "ping line 3", ?ADMIN_LOGOUT],
         "command: execute usual command by admin"},
        {["login", "guest", "idclip", "ping", "logout"],
         %"@CliDemo>login\nlogin:guest\npassword:\nsome greeting message\nguest@CliDemo>ping\nCommand's execution is failed due to the following: {parser_fail,[112,105,110,103],{ping,creation_error,bad_args}}\nCommand execution failed. Return code is 255\nguest@CliDemo>logout\nYou are logged out\n@CliDemo>"
         [?LOGIN, ?GREETING, "guest@CliDemo>" ++ ?PING_BAD_ARGS, ?COMMAND_FAIL, ?GUEST_LOGOUT],
         "command: pass wrong command params by user"},
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