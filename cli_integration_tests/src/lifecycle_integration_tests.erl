-module(lifecycle_integration_tests).

-include_lib("eunit/include/eunit.hrl").

-include("integration_tests_defs.hrl").

-define(LOGIN, "@CliDemo>login:password:").
-define(GREETING, "some greeting message").
-define(GUEST_LOGOUT, "guest@CliDemo>You are logged out.").
-define(ADMIN_LOGOUT, "root@CliDemo#You are logged out.").

integration_test_() ->
    [create_integration_test(["login", "guest", "idclip", "logout"], [?LOGIN, ?GREETING, ?GUEST_LOGOUT], "lifecycle: login an logout by user"),
     create_integration_test(["login", "root", "iddqd", "logout"], [?LOGIN, ?GREETING, ?ADMIN_LOGOUT], "lifecycle: login an logout by admin"),
     create_integration_test(["login", "guest", "idclip", "ping", "ping 192.168.0.1", "configure terminal", "logout"],
                             [?LOGIN,
                              ?GREETING,
                              "guest@CliDemo>Command's execution is failed due to the following: {parser_fail,[112,105,110,103],{ping,creation_error,bad_args}}",
                              "Command execution failed. Return code is 255",
                              "guest@CliDemo>ping line 1",
                              "ping line 2",
                              "ping line 3",
                              "guest@CliDemo>Command's execution is failed due to the following: {precondition_check_fail,access_denied}",
                              "Command execution failed. Return code is 255",
                              ?GUEST_LOGOUT],
                             "lifecycle: big example for user"),
     create_integration_test(["login", "root", "iddqd", "configure terminal", "vlan 666", "name somename", "exit", "interface somedevice 0/1", "show vlan", "no switchport access vlan", "logout"],
                             [?LOGIN,
                              ?GREETING,
                              "root@CliDemo#root@CliDemo (config)#root@CliDemo (config-vlan)#root@CliDemo (config-vlan)#root@CliDemo (config)#root@CliDemo (config-if)#show vlan line 1",
                              "show vlan line 2",
                              "show vlan line 3",
                              "root@CliDemo (config-if)#root@CliDemo (config-if)#You are logged out."],
                             "lifecycle: big example for admin")].

-spec create_integration_test(Input :: [string()], Output :: [string()], Description :: string()) ->
    {Description :: string(), fun(() -> 'ok')}.
create_integration_test(Input, Output, Description) ->
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