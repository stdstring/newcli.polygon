%% @author std-string

-module(command_integration_tests).

-include_lib("eunit/include/eunit.hrl").

-include("integration_tests_defs.hrl").

%% ====================================================================
%% Test functions
%% ====================================================================

integration_test_() ->
    integration_tests_common:create_tests_entry([
        {"command: execute command by unauthenticated user",
         ["ping 192.168.0.1"],
         ["@CliDemo>ping 192.168.0.1", ?ACCESS_DENIED, ?COMMAND_FAIL, "@CliDemo>"]},
        {"command: execute usual command by user",
         ["login", "guest", "idclip", "ping 192.168.0.1", "logout"],
         ?GUEST_LOGIN ++ [?GREETING, "guest@CliDemo>ping 192.168.0.1"] ++ ?PING_OUTPUT ++ ?GUEST_LOGOUT},
        {"command: execute usual command by admin",
         ["login", "root", "iddqd", "ping 192.168.0.1", "logout"],
         ?ADMIN_LOGIN ++ [?GREETING, "root@CliDemo#ping 192.168.0.1"] ++ ?PING_OUTPUT ++ ?ADMIN_LOGOUT},
        {"command: pass wrong command params by user",
         ["login", "guest", "idclip", "ping", "logout"],
         ?GUEST_LOGIN ++ [?GREETING, "guest@CliDemo>ping", ?BAD_ARGS, ?COMMAND_FAIL] ++ ?GUEST_LOGOUT},
        {"command: pass wrong command params by admin",
         ["login", "root", "iddqd", "ping", "logout"],
         ?ADMIN_LOGIN ++ [?GREETING, "root@CliDemo#ping", ?BAD_ARGS, ?COMMAND_FAIL] ++ ?ADMIN_LOGOUT},
        {"command: execute unknown command by user",
         ["login", "guest", "idclip", "pong", "logout"],
         ?GUEST_LOGIN ++ [?GREETING, "guest@CliDemo>pong", ?UNKNOWN_COMMAND] ++ ?GUEST_LOGOUT},
        {"command: execute unknown command by admin",
         ["login", "root", "iddqd", "pong", "logout"],
         ?ADMIN_LOGIN ++ [?GREETING, "root@CliDemo#pong", ?UNKNOWN_COMMAND] ++ ?ADMIN_LOGOUT},
        {"command: execute privileged command by user",
         ["login", "guest", "idclip", "configure terminal", "logout"],
         ?GUEST_LOGIN ++ [?GREETING, "guest@CliDemo>configure terminal", ?ACCESS_DENIED, ?COMMAND_FAIL] ++ ?GUEST_LOGOUT},
        {"command: execute privileged command by admin",
         ["login", "root", "iddqd", "configure terminal", "logout"],
         ?ADMIN_LOGIN ++ [?GREETING, "root@CliDemo#configure terminal", "root@CliDemo (config)#logout"] ++ ?ADMIN_LOGOUT_RESULT}]).

%% ====================================================================
%% Internal functions
%% ====================================================================