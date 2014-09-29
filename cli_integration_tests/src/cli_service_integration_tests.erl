-module(cli_service_integration_tests).

-include_lib("eunit/include/eunit.hrl").

-include("integration_tests_defs.hrl").

%% TODO (std_string) : try use include files from cli_frontend
-define(FRONTEND_COMMAND, {command, "?"}).

integration_test_() ->
    Tests = [{"interaction with backend", fun() -> check_backend() end}, {"interaction with frontend", fun() -> check_frontend() end}],
    [{foreach, fun integration_tests_manager:setup/0, fun integration_tests_manager:cleanup/1, Tests}].

check_backend() ->
    CommandsInfo = backend_interact(),
    io:format(user, "CommandsInfo = ~p~n", [CommandsInfo]),
    ok.

check_frontend() ->
    {OutputResponse, EndResponse} = frontend_interact(),
    io:format(user, "Output = ~p~n", [OutputResponse]),
    io:format(user, "End = ~p~n", [EndResponse]),
    ok.

backend_interact() ->
    %% TODO (std_string) : try use include files from cli_backend
    {commands_info_result, CommandsInfo} = gen_server:call({?BACKEND_PROCESS, ?BACKEND_NODE}, {commands_info}),
    CommandsInfo.

frontend_interact() ->
    {ok, Socket} = gen_tcp:connect(?FRONTEND_ENDPOINT_ADDRESS, ?FRONTEND_ENDPOINT_PORT, [binary, {packet, 4}, {active, false}]),
    gen_tcp:send(Socket, term_to_binary(?FRONTEND_COMMAND)),
    {ok, OutputPacket} = gen_tcp:recv(Socket, 0),
    OutputResponse = binary_to_term(OutputPacket),
    {ok, EndPacket} = gen_tcp:recv(Socket, 0),
    EndResponse = binary_to_term(EndPacket),
    {OutputResponse, EndResponse}.