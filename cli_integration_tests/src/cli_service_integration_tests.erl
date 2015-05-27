%% @author std-string

-module(cli_service_integration_tests).

-include_lib("eunit/include/eunit.hrl").

-include("integration_tests_defs.hrl").
-include("message_defs.hrl").

-define(SLEEP, 2000).
-define(TIMEOUT_RESULT, {error, timeout}).
-define(CLOSED_RESULT, {error, closed}).
%% TODO (std_string) : try use include files from cli_service
-define(COMMAND, {command, "ping XXX"}).

%% ====================================================================
%% Test functions
%% ====================================================================

cli_service_with_big_downtime_test() ->
    integration_tests_common:prepare_cli_service_data(),
    Service = integration_tests_common:start_cli_service(),
    {ok, Socket} = gen_tcp:connect(?SERVICE_ENDPOINT_ADDRESS, ?SERVICE_ENDPOINT_PORT, [binary, {packet, 4}, {active, false}]),
    ?assertEqual(?TIMEOUT_RESULT, gen_tcp:recv(Socket, 0, 0)),
    timer:sleep(?SLEEP),
    ?assertEqual(?TIMEOUT_RESULT, gen_tcp:recv(Socket, 0, 0)),
    integration_tests_common:stop_cli_service(Service).

cli_service_without_downtime_test() ->
    {ok, _} = file:copy("service_data/authentication_data", "/tmp/authentication_data"),
    {ok, _} = file:copy("service_data/authorization_data", "/tmp/authorization_data"),
    {ok, _} = file:copy("service_data/cli_fsm_data", "/tmp/cli_fsm_data"),
    {ok, _} = file:copy("service_data/command_data", "/tmp/command_data"),
    {ok, _} = file:copy("service_data/cli_service_without_downtime.conf", "/tmp/cli_service.conf"),
    check_downtime().

cli_service_with_small_downtime_test() ->
    {ok, _} = file:copy("service_data/authentication_data", "/tmp/authentication_data"),
    {ok, _} = file:copy("service_data/authorization_data", "/tmp/authorization_data"),
    {ok, _} = file:copy("service_data/cli_fsm_data", "/tmp/cli_fsm_data"),
    {ok, _} = file:copy("service_data/command_data", "/tmp/command_data"),
    {ok, _} = file:copy("service_data/cli_service_with_small_downtime.conf", "/tmp/cli_service.conf"),
    check_downtime().

integration_test_() ->
    integration_tests_common:prepare_cli_service_data(),
    Tests = [{"interaction with service", fun check_service/0}],
    {foreach, fun integration_tests_common:start_cli_service/0, fun integration_tests_common:stop_cli_service/1, Tests}.

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec check_downtime() -> 'ok'.
check_downtime() ->
    Service = integration_tests_common:start_cli_service(),
    {ok, Socket} = gen_tcp:connect(?SERVICE_ENDPOINT_ADDRESS, ?SERVICE_ENDPOINT_PORT, [binary, {packet, 4}, {active, false}]),
    ?assertEqual(?TIMEOUT_RESULT, gen_tcp:recv(Socket, 0, 0)),
    timer:sleep(?SLEEP),
    {ok, NotificationBinary} = gen_tcp:recv(Socket, 0, 0),
    ?assertMatch({exit, _Message}, binary_to_term(NotificationBinary)),
    ?assertEqual(?CLOSED_RESULT, gen_tcp:recv(Socket, 0, 0)),
    integration_tests_common:stop_cli_service(Service),
    ok.

-spec check_service() -> 'ok'.
check_service() ->
    Messages = interact_with_service(),
    Expected = [?COMMAND_OUTPUT("ping line 1\n"), ?COMMAND_OUTPUT("ping line 2\n"), ?COMMAND_OUTPUT("ping line 3\n"), ?COMMAND_END("guest@CliDemo>")],
    ?assertEqual(Expected, Messages),
    ok.

%% TODO (std_string) : try use include files from cli_service
-spec interact_with_service() -> [tuple()].
interact_with_service() ->
    {ok, Socket} = gen_tcp:connect(?SERVICE_ENDPOINT_ADDRESS, ?SERVICE_ENDPOINT_PORT, [binary, {packet, 4}, {active, false}]),
    ?LOGIN_SUCCESS_RESPONSE(_Greeting) = cli_service_interaction_helper:login(Socket, "guest", "idclip"),
    Messages = cli_service_interaction_helper:sync_exchange_multiple_response(Socket, ?COMMAND_START("ping XXX"), ?COMMAND_END_TAG),
    cli_service_interaction_helper:simple_logout(Socket),
    Messages.