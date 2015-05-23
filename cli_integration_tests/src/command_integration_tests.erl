%% @author std-string

-module(command_integration_tests).

-include_lib("eunit/include/eunit.hrl").

-include("integration_tests_defs.hrl").

%% ====================================================================
%% Test functions
%% ====================================================================

integration_test_() ->
    integration_tests_common:create_tests_entry([
        {"command: execute usual command by user",
         ["guest", "idclip", "ping 192.168.0.1", "logout"],
         ?GUEST_LOGIN ++ [?GREETING, "guest@CliDemo>ping 192.168.0.1"] ++ ?PING_OUTPUT ++ ?GUEST_LOGOUT},
        {"command: execute usual shortcut command by user",
         ["guest", "idclip", "p 192.168.0.1", "l"],
         ?GUEST_LOGIN ++ [?GREETING, "guest@CliDemo>p 192.168.0.1"] ++ ?PING_OUTPUT ++ ?GUEST_LOGOUT_SHORTCUT},
        {"command: execute usual command by admin",
         ["root", "iddqd", "ping 192.168.0.1", "logout"],
         ?ADMIN_LOGIN ++ [?GREETING, "root@CliDemo#ping 192.168.0.1"] ++ ?PING_OUTPUT ++ ?ADMIN_LOGOUT},
        {"command: execute usual shortcut command by admin",
         ["root", "iddqd", "p 192.168.0.1", "l"],
         ?ADMIN_LOGIN ++ [?GREETING, "root@CliDemo#p 192.168.0.1"] ++ ?PING_OUTPUT ++ ?ADMIN_LOGOUT_SHORTCUT},
        {"command: pass wrong command params by user",
         ["guest", "idclip", "ping", "logout"],
         ?GUEST_LOGIN ++ [?GREETING, "guest@CliDemo>ping", ?BAD_ARGS, ?COMMAND_FAIL] ++ ?GUEST_LOGOUT},
        {"command: pass wrong shortcut command params by user",
         ["guest", "idclip", "p", "l"],
         ?GUEST_LOGIN ++ [?GREETING, "guest@CliDemo>p", ?BAD_ARGS, ?COMMAND_FAIL] ++ ?GUEST_LOGOUT_SHORTCUT},
        {"command: pass wrong command params by admin",
         ["root", "iddqd", "ping", "logout"],
         ?ADMIN_LOGIN ++ [?GREETING, "root@CliDemo#ping", ?BAD_ARGS, ?COMMAND_FAIL] ++ ?ADMIN_LOGOUT},
        {"command: pass wrong shortcut command params by admin",
         ["root", "iddqd", "p", "l"],
         ?ADMIN_LOGIN ++ [?GREETING, "root@CliDemo#p", ?BAD_ARGS, ?COMMAND_FAIL] ++ ?ADMIN_LOGOUT_SHORTCUT},
        {"command: execute unknown command by user",
         ["guest", "idclip", "pong", "logout"],
         ?GUEST_LOGIN ++ [?GREETING, "guest@CliDemo>pong", ?UNKNOWN_COMMAND] ++ ?GUEST_LOGOUT},
        {"command: execute unknown command by admin",
         ["root", "iddqd", "pong", "logout"],
         ?ADMIN_LOGIN ++ [?GREETING, "root@CliDemo#pong", ?UNKNOWN_COMMAND] ++ ?ADMIN_LOGOUT},
        {"command: execute privileged command by user",
         ["guest", "idclip", "configure terminal", "logout"],
         ?GUEST_LOGIN ++ [?GREETING, "guest@CliDemo>configure terminal", ?ACCESS_DENIED, ?COMMAND_FAIL] ++ ?GUEST_LOGOUT},
        {"command: execute privileged shortcut command by user",
         ["guest", "idclip", "c t", "l"],
         ?GUEST_LOGIN ++ [?GREETING, "guest@CliDemo>c t", ?ACCESS_DENIED, ?COMMAND_FAIL] ++ ?GUEST_LOGOUT_SHORTCUT},
        {"command: execute privileged command by admin",
         ["root", "iddqd", "configure terminal", "end", "logout"],
         ?ADMIN_LOGIN ++ [?GREETING, ?CONFIG_TERM, "root@CliDemo (config)#end"] ++ ?ADMIN_LOGOUT},
        {"command: execute privileged shortcut command by admin",
         ["root", "iddqd", "c t", "en", "l"],
         ?ADMIN_LOGIN ++ [?GREETING, ?CONFIG_TERM_SHORTCUT, "root@CliDemo (config)#en"] ++ ?ADMIN_LOGOUT_SHORTCUT},
        {"command: execute unsuitable command by admin",
         ["root", "iddqd", "end", "logout"],
         ?ADMIN_LOGIN ++ [?GREETING, "root@CliDemo#end", ?UNSUITABLE_COMMAND, ?COMMAND_FAIL] ++ ?ADMIN_LOGOUT},
        {"command: execute unsuitable shortcut command by admin",
         ["root", "iddqd", "en", "l"],
         ?ADMIN_LOGIN ++ [?GREETING, "root@CliDemo#en", ?UNSUITABLE_COMMAND, ?COMMAND_FAIL] ++ ?ADMIN_LOGOUT_SHORTCUT},
        {"command: execute ambiguous shortcut command by admin",
         ["root", "iddqd", "c t", "e", "end", "l"],
         ?ADMIN_LOGIN ++ [?GREETING, ?CONFIG_TERM_SHORTCUT, "root@CliDemo (config)#e", ?UNKNOWN_COMMAND, "root@CliDemo (config)#end"] ++ ?ADMIN_LOGOUT_SHORTCUT}]).

%% ====================================================================
%% Internal functions
%% ====================================================================