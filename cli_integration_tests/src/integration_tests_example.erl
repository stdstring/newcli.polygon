-module(integration_tests_example).

-include_lib("eunit/include/eunit.hrl").

-include("integration_tests_defs.hrl").

integration_test_() ->
    [{foreach, fun integration_tests_manager:setup/0, fun integration_tests_manager:cleanup/1, [fun(State) -> [fun() -> integration_test(State) end] end]}].

integration_test(#integration_test_state{frontend_cmd = FrontendCmd}) ->
    InputData = "i?\ninterface ?\n",
    ?assertEqual(ok, file:write_file(?INPUT_DATA, InputData)),
    ActualData = os:cmd(FrontendCmd),
    ExpectedData = "@CliDemo>interface\tinterface range\n@CliDemo>interface {interface-id} command\n@CliDemo>",
    ?assertEqual(ExpectedData, ActualData).