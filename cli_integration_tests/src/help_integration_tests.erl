-module(help_integration_tests).

-include_lib("eunit/include/eunit.hrl").

-include("integration_tests_defs.hrl").

integration_test_() ->
    integration_tests_common:create_tests_entry([
        {"help: show all suitable commands", "?bye", ["@CliDemo>?", "login"] ++ ?BYE_OUTPUT},
        {"help: show suitable commands started with l", "l?bye", ["@CliDemo>l?", "login"] ++ ?BYE_OUTPUT},
        %%{"help: show suitable commands started with p", "p?", ["@CliDemo>p"]},
        {"help: show suitable commands started with login", "login?bye", ["@CliDemo>login?", "login"] ++ ?BYE_OUTPUT},
        {"help: show help for login", "login ?bye", ["@CliDemo>login ?", "login help"] ++ ?BYE_OUTPUT}]).