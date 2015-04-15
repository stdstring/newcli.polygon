%% @author std-string

-module(cli_service_integration_tests).

-include_lib("eunit/include/eunit.hrl").

-include("integration_tests_defs.hrl").

-define(SLEEP, 2000).
-define(TIMEOUT_RESULT, {error, timeout}).
-define(CLOSED_RESULT, {error, closed}).
%% TODO (std_string) : try use include files from cli_service
-define(COMMAND, {command, "?"}).

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
    {OutputResponse, EndResponse} = interact_with_service(),
    ?assertEqual({command_err, ?UNSUITABLE_CHAR ++ "\n"}, OutputResponse),
    ?assertEqual({'end',"@CliDemo>"}, EndResponse),
    ok.

-spec interact_with_service() -> {OutputResponse :: {'command_out', CommandOut :: string()}, EndResponse :: {'end', Prompt :: string()}}.
interact_with_service() ->
    %% {ok, Socket} = gen_tcp:connect({127, 0, 0, 1}, 6666, [binary, {packet, 4}, {active, false}]),
    {ok, Socket} = gen_tcp:connect(?SERVICE_ENDPOINT_ADDRESS, ?SERVICE_ENDPOINT_PORT, [binary, {packet, 4}, {active, false}]),
    gen_tcp:send(Socket, term_to_binary(?COMMAND)),
    {ok, OutputPacket} = gen_tcp:recv(Socket, 0),
    OutputResponse = binary_to_term(OutputPacket),
    {ok, EndPacket} = gen_tcp:recv(Socket, 0),
    EndResponse = binary_to_term(EndPacket),
    {OutputResponse, EndResponse}.