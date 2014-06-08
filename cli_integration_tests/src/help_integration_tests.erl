-module(help_integration_tests).

-include_lib("eunit/include/eunit.hrl").

-include("integration_tests_defs.hrl").

integration_test_() ->
    [{foreach, fun integration_tests_manager:setup/0, fun integration_tests_manager:cleanup/1,
        [fun(State) -> [{"help: show single variant of commands", fun() -> integration_tests_common:process("p?\n", "@CliDemo>ping\n@CliDemo>", State) end}] end,
         fun(State) -> [{"help: show several variants of commands", fun() -> integration_tests_common:process("i?\n", "@CliDemo>interface\tinterface range\n@CliDemo>", State) end}] end]}].
    %%[integration_tests_common:create_test_entry("p?\n", "@CliDemo>ping\n@CliDemo>", "help: show single variant of commands"),
    %%integration_tests_common:create_test_entry("i?\n", "@CliDemo>interface\tinterface range\n@CliDemo>", "help: show several variants of commands")].