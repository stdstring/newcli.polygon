-module(integration_tests_example).

-include_lib("eunit/include/eunit.hrl").

-include("integration_tests_defs.hrl").

integration_test_() ->
    [{foreach, fun integration_tests_manager:setup/0, fun integration_tests_manager:cleanup/1, [fun(State) -> [fun() -> integration_test(State) end] end]}].

integration_test(State) ->
    Input = "i?\ninterface ?\n",
    ExpectedOutput = "@CliDemo>interface\tinterface range\n@CliDemo>interface {interface-id} command\n@CliDemo>",
    integration_tests_common:process(Input, ExpectedOutput, State).