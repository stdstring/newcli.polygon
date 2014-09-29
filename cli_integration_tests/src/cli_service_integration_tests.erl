-module(cli_service_integration_tests).

-include_lib("eunit/include/eunit.hrl").

-include("integration_tests_defs.hrl").

%% TODO (std_string) : try use include files from cli_backend
-define(BACKEND_COMMAND, {commands_info}).
%% TODO (std_string) : try use include files from cli_frontend
-define(FRONTEND_COMMAND, {command, "?"}).

-define(EXPECTED_COMMANDS, ["login", "logout", "configure terminal", "interface", "interface range", "vlan", "no vlan", "ping", "switchport access vlan", "no switchport access vlan", "name", "no name", "end", "exit", "show vlan"]).

integration_test_() ->
    Tests = [{"interaction with backend", fun() -> check_backend() end}, {"interaction with frontend", fun() -> check_frontend() end}],
    [{foreach, fun integration_tests_manager:setup/0, fun integration_tests_manager:cleanup/1, Tests}].

check_backend() ->
    CommandsInfo = backend_interact(),
    ?assertEqual(length(?EXPECTED_COMMANDS), length(CommandsInfo)),
    lists:foreach(fun(Command) -> ?assert(is_backend_command_exist(CommandsInfo, Command)) end, ?EXPECTED_COMMANDS),
    ok.

is_backend_command_exist(CommandsInfo, ExpectedCommand) ->
    %% TODO (std_string) : try use include files from cli_backend
    Pred = fun({command_info, _Name, Body, _Help}) -> string:join(Body, " ") == ExpectedCommand end,
    lists:any(Pred, CommandsInfo).

check_frontend() ->
    {OutputResponse, EndResponse} = frontend_interact(),
    %% TODO (std_string) : try use include files from cli_frontend
    {command_out, CommandOut} = OutputResponse,
    Commands = string:tokens(string:strip(CommandOut, right, $\n), "\t"),
    lists:foreach(fun(Command) -> ?assert(is_frontend_command_exist(Commands, Command)) end, ?EXPECTED_COMMANDS),
    ?assertEqual({'end',"@CliDemo>"}, EndResponse),
    ok.

is_frontend_command_exist(Commands, ExpectedCommand) ->
    lists:any(fun(Command) -> Command == ExpectedCommand end, Commands).


backend_interact() ->
    %% TODO (std_string) : try use include files from cli_backend
    {commands_info_result, CommandsInfo} = gen_server:call({?BACKEND_PROCESS, ?BACKEND_NODE}, ?BACKEND_COMMAND),
    CommandsInfo.

frontend_interact() ->
    {ok, Socket} = gen_tcp:connect(?FRONTEND_ENDPOINT_ADDRESS, ?FRONTEND_ENDPOINT_PORT, [binary, {packet, 4}, {active, false}]),
    gen_tcp:send(Socket, term_to_binary(?FRONTEND_COMMAND)),
    {ok, OutputPacket} = gen_tcp:recv(Socket, 0),
    OutputResponse = binary_to_term(OutputPacket),
    {ok, EndPacket} = gen_tcp:recv(Socket, 0),
    EndResponse = binary_to_term(EndPacket),
    {OutputResponse, EndResponse}.