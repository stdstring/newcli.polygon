-module(integration_tests_example).

-include_lib("eunit/include/eunit.hrl").

-include("integration_tests_defs.hrl").

integration_test_() ->
    [integration_tests_common:create_test_entry("i?\ninterface ?\n", "@CliDemo>interface\tinterface range\n@CliDemo>interface {interface-id} command\n@CliDemo>", "integration test example")].