-module(help_integration_tests).

-include_lib("eunit/include/eunit.hrl").

-include("integration_tests_defs.hrl").

integration_test_() ->
    integration_tests_common:create_tests_entry([
        {["p?"],
         %"@CliDemo>p?\nping\n@CliDemo>"
         ["@CliDemo>ping"],
         "help: show single variant of commands"},
        {["i?"],
         %"@CliDemo>i?\ninterface\tinterface range\n@CliDemo>"
         ["@CliDemo>interface\tinterface range"],
         "help: show several variants of commands"},
        {["iddqd?"],
         %"@CliDemo>iddqd?\nCommands not found\n@CliDemo>"
         ["@CliDemo>Commands not found."],
         "help: show variants for unknown command prefix"},
        {["interface ?"],
         %"@CliDemo>interface ?\ninterface {interface-id} command\n@CliDemo>"
         ["@CliDemo>interface {interface-id} command"],
         "help: show help for command"},
        {["idkfa ?"],
         %"@CliDemo>idkfa ?\nHelp not found\n@CliDemo>"
         ["@CliDemo>Help not found."],
         "help: show help for unknown command"}]).