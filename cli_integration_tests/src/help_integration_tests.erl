-module(help_integration_tests).

-include_lib("eunit/include/eunit.hrl").

-include("integration_tests_defs.hrl").

integration_test_() ->
    integration_tests_common:create_tests_entry([{["p?"], ["@CliDemo>ping"], "help: show single variant of commands"},
                                                 {["i?"], ["@CliDemo>interface\tinterface range"], "help: show several variants of commands"}]).