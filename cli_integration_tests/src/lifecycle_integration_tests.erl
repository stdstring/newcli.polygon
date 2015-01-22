-module(lifecycle_integration_tests).

-include_lib("eunit/include/eunit.hrl").

-include("integration_tests_defs.hrl").

-define(SHOW_VLAN_OUTPUT, ["show vlan line 1", "show vlan line 2", "show vlan line 3"]).

integration_test_() ->
    [create_integration_test(
        "lifecycle: login and logout by user",
        ["login", "guest", "idclip", "logout"],
        ?LOGIN("guest") ++ [?GREETING] ++ ?GUEST_LOGOUT),
     create_integration_test(
        "lifecycle: login and logout by admin",
        ["login", "root", "iddqd", "logout"],
        ?LOGIN("root") ++ [?GREETING] ++ ?ADMIN_LOGOUT),
     create_integration_test(
        "lifecycle: big example for user",
        ["login", "guest", "idclip", "ping", "ping 192.168.0.1", "configure terminal", "logout"],
        create_big_example_user_output()),
     create_integration_test(
        "lifecycle: big example for admin",
        create_big_example_admin_input(),
        create_big_example_admin_output())].

-spec create_big_example_user_output() -> [string()].
create_big_example_user_output() ->
    ?LOGIN("guest") ++
    [?GREETING] ++
    ["guest@CliDemo>ping"] ++
    [?BAD_ARGS] ++
    [?COMMAND_FAIL] ++
    ["guest@CliDemo>ping 192.168.0.1"] ++
    ?PING_OUTPUT ++
    ["guest@CliDemo>configure terminal"] ++
    [?ACCESS_DENIED] ++
    [?COMMAND_FAIL] ++
    ?GUEST_LOGOUT.

-spec create_big_example_admin_input() -> [string()].
create_big_example_admin_input() ->
    ["login",
     "root",
     "iddqd",
     "vlan 666",
     "configure terminal",
     "vlan 666",
     "name somename",
     "exit",
     "interface someinterface 0/1",
     "show vlan",
     "no switchport access vlan",
     "logout"].

-spec create_big_example_admin_output() -> [string()].
create_big_example_admin_output() ->
    ?LOGIN("root") ++
    [?GREETING] ++
    ["root@CliDemo#vlan 666"] ++
    [?UNSUITABLE_COMMAND] ++
    [?COMMAND_FAIL] ++
    [?CONFIG_TERM] ++
    [?VLAN] ++
    ["root@CliDemo (config-vlan)#name somename"] ++
    ["root@CliDemo (config-vlan)#exit"] ++
    [?INTERFACE] ++
    ["root@CliDemo (config-if)#show vlan"] ++
    ?SHOW_VLAN_OUTPUT ++
    ["root@CliDemo (config-if)#no switchport access vlan"] ++
    ["root@CliDemo (config-if)#logout"] ++
    ?LOGOUT_RESULT("root").

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