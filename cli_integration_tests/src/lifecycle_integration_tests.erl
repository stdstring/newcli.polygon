-module(lifecycle_integration_tests).

-include_lib("eunit/include/eunit.hrl").

-include("integration_tests_defs.hrl").

integration_test_() ->
    [create_integration_test(["login", "guest", "idclip", "logout"],
                             %"@CliDemo>login\nlogin:guest\npassword:\nsome greeting message\nguest@CliDemo>logout\nYou are logged out\n@CliDemo>"
                             [?LOGIN, ?GREETING, ?GUEST_LOGOUT],
                             "lifecycle: login and logout by user"),
     create_integration_test(["login", "root", "iddqd", "logout"],
                             %"@CliDemo>login\nlogin:root\npassword:\nsome greeting message\nroot@CliDemo#logout\nYou are logged out\n@CliDemo>"
                             [?LOGIN, ?GREETING, ?ADMIN_LOGOUT],
                             "lifecycle: login and logout by admin"),
     create_integration_test(["login", "guest", "idclip", "ping", "ping 192.168.0.1", "configure terminal", "logout"],
                             %"@CliDemo>login\nlogin:guest\npassword:\nsome greeting message\nguest@CliDemo>ping\nCommand's execution is failed due to the following: {parser_fail,[112,105,110,103],{ping,creation_error,bad_args}}\nCommand execution failed. Return code is 255\nguest@CliDemo>ping 192.168.0.1\nping line 1\nping line 2\nping line 3\nguest@CliDemo>configure terminal\nCommand's execution is failed due to the following: {precondition_check_fail,access_denied}\nCommand execution failed. Return code is 255\nguest@CliDemo>logout\nYou are logged out\n@CliDemo>"
                             [?LOGIN,
                              ?GREETING,
                              "guest@CliDemo>" ++ ?PING_BAD_ARGS,
                              ?COMMAND_FAIL,
                              "guest@CliDemo>ping line 1",
                              "ping line 2",
                              "ping line 3",
                              "guest@CliDemo>" ++ ?ACCESS_DENIED,
                              ?COMMAND_FAIL,
                              ?GUEST_LOGOUT],
                             "lifecycle: big example for user"),
     create_integration_test(["login", "root", "iddqd", "vlan 666", "configure terminal", "vlan 666", "name somename", "exit", "interface somedevice 0/1", "show vlan", "no switchport access vlan", "logout"],
                             %"@CliDemo>login\nlogin:root\npassword:\nsome greeting message\nroot@CliDemo#vlan 666\nCommand's execution is failed due to the following: {precondition_check_fail,unsuitable_command}\nCommand execution failed. Return code is 255\nroot@CliDemo#configure terminal\nroot@CliDemo (config)#vlan 666\nroot@CliDemo (config-vlan)#name somename\nroot@CliDemo (config-vlan)#exit\nroot@CliDemo (config)#interface somedevice 0/1\nroot@CliDemo (config-if)#show vlan\nshow vlan line 1\nshow vlan line 2\nshow vlan line 3\nroot@CliDemo (config-if)#no switchport access vlan\nroot@CliDemo (config-if)#logout\nYou are logged out\n@CliDemo>"
                             [?LOGIN,
                              ?GREETING,
                              "root@CliDemo#" ++ ?UNSUITABLE_COMMAND,
                              ?COMMAND_FAIL,
                              "root@CliDemo#root@CliDemo (config)#root@CliDemo (config-vlan)#root@CliDemo (config-vlan)#root@CliDemo (config)#root@CliDemo (config-if)#show vlan line 1",
                              "show vlan line 2",
                              "show vlan line 3",
                              "root@CliDemo (config-if)#root@CliDemo (config-if)#You are logged out."],
                             "lifecycle: big example for admin")].

-spec create_integration_test(Description :: string(), Input :: [string()], Output :: [string()]) ->
    {Description :: string(), fun(() -> 'ok')}.
create_integration_test(Description, Input, Output) ->
    {Description, fun() -> test_common_body(Input, Output) end}.

-spec test_common_body(Input :: [string()], Output :: [string()]) -> 'ok'.
test_common_body(Input, Output) ->
    State = integration_tests_manager:setup(),
    try
        integration_tests_common:process(Input, Output, State)
    after
        integration_tests_manager:cleanup(State)
    end,
    integration_tests_common:check_normal_execution(),
    ok.