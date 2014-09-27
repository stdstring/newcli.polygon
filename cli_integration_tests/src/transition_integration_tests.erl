-module(transition_integration_tests).

-include_lib("eunit/include/eunit.hrl").

-include("integration_tests_defs.hrl").

integration_test_() ->
    integration_tests_common:create_tests_entry([
        {["login", "root", "iddqd", "no name", "logout"],
         %"@CliDemo>login\nlogin:root\npassword:\nsome greeting message\nroot@CliDemo#no name\nCommand's execution is failed due to the following: {precondition_check_fail,unsuitable_command}\nCommand execution failed. Return code is 255\nroot@CliDemo#logout\nYou are logged out\n@CliDemo>"
         [?LOGIN, ?GREETING, "root@CliDemo#" ++ ?UNSUITABLE_COMMAND, ?COMMAND_FAIL, ?ADMIN_LOGOUT],
         "transition: execute unsuitable command"},
        {["login", "root", "iddqd", "configure terminal", "logout"],
         %"@CliDemo>login\nlogin:root\npassword:\nsome greeting message\nroot@CliDemo#configure terminal\nroot@CliDemo (config)#logout\nYou are logged out\n@CliDemo>"
         [?LOGIN, ?GREETING, "root@CliDemo#root@CliDemo (config)#You are logged out."],
         "transition: enter into global configuration mode"},
        {["login", "root", "iddqd", "configure terminal", "interface someinterface 0/1", "logout"],
         %"@CliDemo>login\nlogin:root\npassword:\nsome greeting message\nroot@CliDemo#configure terminal\nroot@CliDemo (config)#interface someinterface 0/1\nroot@CliDemo (config-if)#logout\nYou are logged out\n@CliDemo>"
         [?LOGIN, ?GREETING, "root@CliDemo#root@CliDemo (config)#root@CliDemo (config-if)#You are logged out."],
         "transition: enter into interface configuration mode"},
        {["login", "root", "iddqd", "configure terminal", "interface range someinterface 0/1,3-5", "logout"],
         %"@CliDemo>login\nlogin:root\npassword:\nsome greeting message\nroot@CliDemo#configure terminal\nroot@CliDemo (config)#interface range someinterface 0/1,3-5\nroot@CliDemo (config-if-range)#logout\nYou are logged out\n@CliDemo>"
         [?LOGIN, ?GREETING, "root@CliDemo#root@CliDemo (config)#root@CliDemo (config-if-range)#You are logged out."],
         "transition: enter into interface range configuration mode"},
        {["login", "root", "iddqd", "configure terminal", "vlan 666", "logout"],
         %"@CliDemo>login\nlogin:root\npassword:\nsome greeting message\nroot@CliDemo#configure terminal\nroot@CliDemo (config)#vlan 666\nroot@CliDemo (config-vlan)#logout\nYou are logged out\n@CliDemo>"
         [?LOGIN, ?GREETING, "root@CliDemo#root@CliDemo (config)#root@CliDemo (config-vlan)#You are logged out."],
         "transition: enter into vlan configuration mode"},
        {["login", "root", "iddqd", "configure terminal", "interface someinterface 0/1", "exit", "logout"],
         %"@CliDemo>login\nlogin:root\npassword:\nsome greeting message\nroot@CliDemo#configure terminal\nroot@CliDemo (config)#interface someinterface 0/1\nroot@CliDemo (config-if)#exit\nroot@CliDemo (config)#logout\nYou are logged out\n@CliDemo>"
         [?LOGIN, ?GREETING, "root@CliDemo#root@CliDemo (config)#root@CliDemo (config-if)#root@CliDemo (config)#You are logged out."],
         "transition: exit from interface configuration mode"},
        {["login", "root", "iddqd", "configure terminal", "interface range someinterface 0/1,3-5", "exit", "logout"],
         %"@CliDemo>login\nlogin:root\npassword:\nsome greeting message\nroot@CliDemo#configure terminal\nroot@CliDemo (config)#interface range someinterface 0/1,3-5\nroot@CliDemo (config-if-range)#exit\nroot@CliDemo (config)#logout\nYou are logged out\n@CliDemo>"
         [?LOGIN, ?GREETING, "root@CliDemo#root@CliDemo (config)#root@CliDemo (config-if-range)#root@CliDemo (config)#You are logged out."],
         "transition: exit from interface range configuration mode"},
        {["login", "root", "iddqd", "configure terminal", "vlan 666", "exit", "logout"],
         %"@CliDemo>login\nlogin:root\npassword:\nsome greeting message\nroot@CliDemo#configure terminal\nroot@CliDemo (config)#vlan 666\nroot@CliDemo (config-vlan)#exit\nroot@CliDemo (config)#logout\nYou are logged out\n@CliDemo>"
         [?LOGIN, ?GREETING, "root@CliDemo#root@CliDemo (config)#root@CliDemo (config-vlan)#root@CliDemo (config)#You are logged out."],
         "transition: exit from vlan configuration mode"},
        {["login", "root", "iddqd", "configure terminal", "exit", "logout"],
         %"@CliDemo>login\nlogin:root\npassword:\nsome greeting message\nroot@CliDemo#configure terminal\nroot@CliDemo (config)#exit\nroot@CliDemo#logout\nYou are logged out\n@CliDemo>"
         [?LOGIN, ?GREETING, "root@CliDemo#root@CliDemo (config)#root@CliDemo#You are logged out."],
         "transition: exit from global configuration mode"},
        {["login", "root", "iddqd", "exit", "logout"],
         %"@CliDemo>login\nlogin:root\npassword:\nsome greeting message\nroot@CliDemo#exit\nCommand's execution is failed due to the following: {precondition_check_fail,unsuitable_command}\nCommand execution failed. Return code is 255\nroot@CliDemo#logout\nYou are logged out\n@CliDemo>"
         [?LOGIN, ?GREETING, "root@CliDemo#" ++ ?UNSUITABLE_COMMAND, ?COMMAND_FAIL, ?ADMIN_LOGOUT],
         "transition: exit from fundamental mode"},
        {["login", "root", "iddqd", "configure terminal", "interface someinterface 0/1", "end", "logout"],
         %"@CliDemo>login\nlogin:root\npassword:\nsome greeting message\nroot@CliDemo#configure terminal\nroot@CliDemo (config)#interface someinterface 0/1\nroot@CliDemo (config-if)#end\nroot@CliDemo#logout\nYou are logged out\n@CliDemo>"
         [?LOGIN, ?GREETING, "root@CliDemo#root@CliDemo (config)#root@CliDemo (config-if)#root@CliDemo#You are logged out."],
         "transition: end interface configuration mode"},
        {["login", "root", "iddqd", "configure terminal", "interface range someinterface 0/1,3-5", "end", "logout"],
         %"@CliDemo>login\nlogin:root\npassword:\nsome greeting message\nroot@CliDemo#configure terminal\nroot@CliDemo (config)#interface range someinterface 0/1,3-5\nroot@CliDemo (config-if-range)#end\nroot@CliDemo#logout\nYou are logged out\n@CliDemo>"
         [?LOGIN, ?GREETING, "root@CliDemo#root@CliDemo (config)#root@CliDemo (config-if-range)#root@CliDemo#You are logged out."],
         "transition: end interface range configuration mode"},
        {["login", "root", "iddqd", "configure terminal", "vlan 666", "end", "logout"],
         %"@CliDemo>login\nlogin:root\npassword:\nsome greeting message\nroot@CliDemo#configure terminal\nroot@CliDemo (config)#vlan 666\nroot@CliDemo (config-vlan)#end\nroot@CliDemo#logout\nYou are logged out\n@CliDemo>"
         [?LOGIN, ?GREETING, "root@CliDemo#root@CliDemo (config)#root@CliDemo (config-vlan)#root@CliDemo#You are logged out."],
         "transition: end vlan configuration mode"},
        {["login", "root", "iddqd", "configure terminal", "end", "logout"],
         %"@CliDemo>login\nlogin:root\npassword:\nsome greeting message\nroot@CliDemo#configure terminal\nroot@CliDemo (config)#end\nroot@CliDemo#logout\nYou are logged out\n@CliDemo>"
         [?LOGIN, ?GREETING, "root@CliDemo#root@CliDemo (config)#root@CliDemo#You are logged out."],
         "transition: end global configuration mode"},
        {["login", "root", "iddqd", "end", "logout"],
         %"@CliDemo>login\nlogin:root\npassword:\nsome greeting message\nroot@CliDemo#end\nCommand's execution is failed due to the following: {precondition_check_fail,unsuitable_command}\nCommand execution failed. Return code is 255\nroot@CliDemo#logout\nYou are logged out\n@CliDemo>"
         [?LOGIN, ?GREETING, "root@CliDemo#" ++ ?UNSUITABLE_COMMAND, ?COMMAND_FAIL, ?ADMIN_LOGOUT],
         "transition: end fundamental mode"}]).