-module(cli_service_integration_tests).

-include_lib("eunit/include/eunit.hrl").

-include("integration_tests_defs.hrl").

%% TODO (std_string) : try use include files from cli_service
-define(COMMAND, {command, "?"}).

%% ====================================================================
%% Test functions
%% ====================================================================

sample_test() ->
    Service = integration_tests_common:start_cli_service(),
    integration_tests_common:stop_cli_service(Service).

integration_test_() ->
    Tests = [{"interaction with service", fun check_service/0}],
    {foreach, fun integration_tests_common:start_cli_service/0, fun integration_tests_common:stop_cli_service/1, Tests}.

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec check_service() -> 'ok'.
check_service() ->
    {OutputResponse, EndResponse} = interact_with_service(),
    io:format(user, "Output = ~p~n", [OutputResponse]),
    io:format(user, "End = ~p~n", [EndResponse]),
    ?assertEqual({command_err,"Command's creation is failed due to the following reason: enotsup\n"}, OutputResponse),
    ?assertEqual({'end',"@>"}, EndResponse),
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