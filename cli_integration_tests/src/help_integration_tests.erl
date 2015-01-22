-module(help_integration_tests).

-include_lib("eunit/include/eunit.hrl").

-include("integration_tests_defs.hrl").

integration_test_() ->
    integration_tests_common:create_tests_entry([
        {"help", ["p?"], ["@CliDemo>p?", "Command's creation is failed due to the following reason: enotsup", "@CliDemo>"]}]).
%%        {"help: show single variant of commands",
%%         ["p?"],
%%         ["@CliDemo>p?", "ping", "@CliDemo>"]},
%%        {"help: show several variants of commands",
%%         ["i?"],
%%         ["@CliDemo>i?", "interface\tinterface range", "@CliDemo>"]},
%%        {"help: show variants for unknown command prefix",
%%         ["iddqd?"],
%%         ["@CliDemo>iddqd?", "Commands not found", "@CliDemo>"]},
%%        {"help: show help for command",
%%         ["interface ?"],
%%         ["@CliDemo>interface ?", "interface {interface-id} command", "@CliDemo>"]},
%%        {"help: show help for unknown command",
%%         ["idkfa ?"],
%%         ["@CliDemo>idkfa ?", "Help not found", "@CliDemo>"]}]).