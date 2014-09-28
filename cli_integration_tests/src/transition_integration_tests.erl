-module(transition_integration_tests).

-include_lib("eunit/include/eunit.hrl").

-include("integration_tests_defs.hrl").

-define(CONFIG_LOGOUT, ["root@CliDemo (config)#logout"] ++ ?LOGOUT_RESULT).

integration_test_() ->
    integration_tests_common:create_tests_entry([
        {"transition: execute unsuitable command",
         ["login", "root", "iddqd", "no name", "logout"],
         ?ADMIN_LOGIN ++ [?GREETING, "root@CliDemo#no name", ?UNSUITABLE_COMMAND, ?COMMAND_FAIL] ++ ?ADMIN_LOGOUT},
        {"transition: enter into global configuration mode",
         ["login", "root", "iddqd", "configure terminal", "logout"],
         ?ADMIN_LOGIN ++ [?GREETING, ?CONFIG_TERM] ++ ?CONFIG_LOGOUT},
        {"transition: enter into interface configuration mode",
         ["login", "root", "iddqd", "configure terminal", "interface someinterface 0/1", "logout"],
         ?ADMIN_LOGIN ++ [?GREETING, ?CONFIG_TERM, ?INTERFACE, "root@CliDemo (config-if)#logout"] ++ ?LOGOUT_RESULT},
        {"transition: enter into interface range configuration mode",
         ["login", "root", "iddqd", "configure terminal", "interface range someinterface 0/1,3-5", "logout"],
         ?ADMIN_LOGIN ++ [?GREETING, ?CONFIG_TERM, ?INTERFACE_RANGE, "root@CliDemo (config-if-range)#logout"] ++ ?LOGOUT_RESULT},
        {"transition: enter into vlan configuration mode",
         ["login", "root", "iddqd", "configure terminal", "vlan 666", "logout"],
         ?ADMIN_LOGIN ++ [?GREETING, ?CONFIG_TERM, ?VLAN, "root@CliDemo (config-vlan)#logout"] ++ ?LOGOUT_RESULT},
        {"transition: exit from interface configuration mode",
         ["login", "root", "iddqd", "configure terminal", "interface someinterface 0/1", "exit", "logout"],
         ?ADMIN_LOGIN ++ [?GREETING, ?CONFIG_TERM, ?INTERFACE, "root@CliDemo (config-if)#exit"] ++ ?CONFIG_LOGOUT},
        {"transition: exit from interface range configuration mode",
         ["login", "root", "iddqd", "configure terminal", "interface range someinterface 0/1,3-5", "exit", "logout"],
         ?ADMIN_LOGIN ++ [?GREETING, ?CONFIG_TERM, ?INTERFACE_RANGE, "root@CliDemo (config-if-range)#exit"] ++ ?CONFIG_LOGOUT},
        {"transition: exit from vlan configuration mode",
         ["login", "root", "iddqd", "configure terminal", "vlan 666", "exit", "logout"],
         ?ADMIN_LOGIN ++ [?GREETING, ?CONFIG_TERM, ?VLAN, "root@CliDemo (config-vlan)#exit"] ++ ?CONFIG_LOGOUT},
        {"transition: exit from global configuration mode",
         ["login", "root", "iddqd", "configure terminal", "exit", "logout"],
         ?ADMIN_LOGIN ++ [?GREETING, ?CONFIG_TERM, "root@CliDemo (config)#exit"] ++ ?ADMIN_LOGOUT},
        {"transition: exit from fundamental mode",
         ["login", "root", "iddqd", "exit", "logout"],
         ?ADMIN_LOGIN ++ [?GREETING, "root@CliDemo#exit", ?UNSUITABLE_COMMAND, ?COMMAND_FAIL] ++ ?ADMIN_LOGOUT},
        {"transition: end interface configuration mode",
         ["login", "root", "iddqd", "configure terminal", "interface someinterface 0/1", "end", "logout"],
         ?ADMIN_LOGIN ++ [?GREETING, ?CONFIG_TERM, ?INTERFACE, "root@CliDemo (config-if)#end"] ++ ?ADMIN_LOGOUT},
        {"transition: end interface range configuration mode",
         ["login", "root", "iddqd", "configure terminal", "interface range someinterface 0/1,3-5", "end", "logout"],
         ?ADMIN_LOGIN ++ [?GREETING, ?CONFIG_TERM, ?INTERFACE_RANGE, "root@CliDemo (config-if-range)#end"] ++ ?ADMIN_LOGOUT},
        {"transition: end vlan configuration mode",
         ["login", "root", "iddqd", "configure terminal", "vlan 666", "end", "logout"],
         ?ADMIN_LOGIN ++ [?GREETING, ?CONFIG_TERM, ?VLAN "root@CliDemo (config-vlan)#end"] ++ ?ADMIN_LOGOUT},
        {"transition: end global configuration mode",
         ["login", "root", "iddqd", "configure terminal", "end", "logout"],
         ?ADMIN_LOGIN ++ [?GREETING, ?CONFIG_TERM, "root@CliDemo (config)#end"] ++ ?ADMIN_LOGOUT},
        {"transition: end fundamental mode",
         ["login", "root", "iddqd", "end", "logout"],
         ?ADMIN_LOGIN ++ [?GREETING, "root@CliDemo#end", ?UNSUITABLE_COMMAND, ?COMMAND_FAIL] ++ ?ADMIN_LOGOUT}]).