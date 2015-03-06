-module(help_integration_tests).

-include_lib("eunit/include/eunit.hrl").

-include("integration_tests_defs.hrl").

integration_test_() ->
    integration_tests_common:create_tests_entry([
        {"help: show all suitable commands", "?", ["@CliDemo>?", "login", "@CliDemo>"]},
        {"help: show suitable commands started with l", "l?", ["@CliDemo>l?", "login", "@CliDemo>"]},
        {"help: show suitable commands started with p", "p?", ["@CliDemo>p"]},
        {"help: show suitable commands started with login", "login?", ["@CliDemo>login?", "login", "@CliDemo>"]},
        {"help: show help for login", "login ?", ["@CliDemo>login ?", "login help", "@CliDemo>"]}
        ]).