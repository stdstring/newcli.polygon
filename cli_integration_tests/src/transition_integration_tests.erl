-module(transition_integration_tests).

-include_lib("eunit/include/eunit.hrl").

-include("integration_tests_defs.hrl").

-define(LOGIN, "@CliDemo>login:password:").
-define(GREETING, "some greeting message").
-define(GUEST_LOGOUT, "guest@CliDemo>You are logged out.").
-define(ADMIN_LOGOUT, "root@CliDemo#You are logged out.").

integration_test_() ->
    integration_tests_common:create_tests_entry([
        {["login", "root", "iddqd", "interface 0/1", "logout"], [?LOGIN, ?GREETING, "root@CliDemo#Command's execution is failed due to the following: {precondition_check_fail,unsuitable_command}", "Command execution failed. Return code is 255", ?ADMIN_LOGOUT], "transition: execute unsuitable command"},
        {["login", "root", "iddqd", "configure terminal", "logout"], [?LOGIN, ?GREETING, "root@CliDemo#root@CliDemo (config)#You are logged out."], "transition: enter into global configuration mode"},
        {["login", "root", "iddqd", "configure terminal", "interface someinterface 0/1", "logout"], [?LOGIN, ?GREETING, "root@CliDemo#root@CliDemo (config)#root@CliDemo (config-if)#You are logged out."], "transition: enter into interface configuration mode"},
        {["login", "root", "iddqd", "configure terminal", "interface range someinterface 0/1,3-5", "logout"], [?LOGIN, ?GREETING, "root@CliDemo#root@CliDemo (config)#root@CliDemo (config-if-range)#You are logged out."], "transition: enter into interface range configuration mode"},
        {["login", "root", "iddqd", "configure terminal", "vlan 666", "logout"], [?LOGIN, ?GREETING, "root@CliDemo#root@CliDemo (config)#root@CliDemo (config-vlan)#You are logged out."], "transition: enter into vlan configuration mode"},
        {["login", "root", "iddqd", "configure terminal", "interface someinterface 0/1", "exit", "logout"], [?LOGIN, ?GREETING, "root@CliDemo#root@CliDemo (config)#root@CliDemo (config-if)#root@CliDemo (config)#You are logged out."], "transition: exit from interface configuration mode"},
        {["login", "root", "iddqd", "configure terminal", "interface range someinterface 0/1,3-5", "exit", "logout"], [?LOGIN, ?GREETING, "root@CliDemo#root@CliDemo (config)#root@CliDemo (config-if-range)#root@CliDemo (config)#You are logged out."], "transition: exit from interface range configuration mode"},
        {["login", "root", "iddqd", "configure terminal", "vlan 666", "exit", "logout"], [?LOGIN, ?GREETING, "root@CliDemo#root@CliDemo (config)#root@CliDemo (config-vlan)#root@CliDemo (config)#You are logged out."], "transition: exit from vlan configuration mode"},
        {["login", "root", "iddqd", "configure terminal", "exit", "logout"], [?LOGIN, ?GREETING, "root@CliDemo#root@CliDemo (config)#root@CliDemo#You are logged out."], "transition: exit from global configuration mode"},
        {["login", "root", "iddqd", "configure terminal", "interface someinterface 0/1", "end", "logout"], [?LOGIN, ?GREETING, "root@CliDemo#root@CliDemo (config)#root@CliDemo (config-if)#root@CliDemo#You are logged out."], "transition: end interface configuration mode"},
        {["login", "root", "iddqd", "configure terminal", "interface range someinterface 0/1,3-5", "end", "logout"], [?LOGIN, ?GREETING, "root@CliDemo#root@CliDemo (config)#root@CliDemo (config-if-range)#root@CliDemo#You are logged out."], "transition: end interface range configuration mode"},
        {["login", "root", "iddqd", "configure terminal", "vlan 666", "end", "logout"], [?LOGIN, ?GREETING, "root@CliDemo#root@CliDemo (config)#root@CliDemo (config-vlan)#root@CliDemo#You are logged out."], "transition: end vlan configuration mode"},
        {["login", "root", "iddqd", "configure terminal", "end", "logout"], [?LOGIN, ?GREETING, "root@CliDemo#root@CliDemo (config)#root@CliDemo#You are logged out."], "transition: end global configuration mode"}]).