-module(autocomplete_factory_tests).

-include_lib("eunit/include/eunit.hrl").

%% ====================================================================
%% Test functions
%% ====================================================================

process_autocomplete_test_() ->
    CommandsBody =[
        ["set"],
        ["set", "global"],
        ["set", "global", "var"],
        ["set", "local"],
        ["clear"]],
    ExtensionsGenerator = autocomplete_factory:create_extension_generator(CommandsBody),
    [{"extensions for unknown command", ?_assertEqual([], ExtensionsGenerator("do it"))},
     {"single extension for clear", ?_assertEqual(["clear"], ExtensionsGenerator("clear"))},
     {"multiple extensions for set", ?_assertEqual(["set", "set global", "set global var", "set local"], ExtensionsGenerator("set"))},
     {"multiple extensions for set global", ?_assertEqual(["set global", "set global var"], ExtensionsGenerator("set global"))},
     {"single extension for set global var", ?_assertEqual(["set global var"], ExtensionsGenerator("set global var"))},
     {"single extension for set local", ?_assertEqual(["set local"], ExtensionsGenerator("set local"))},
     {"extensions for set with parameters", ?_assertEqual([], ExtensionsGenerator("set -v 33"))},
     {"multiple extensions for set global with many spaces", ?_assertEqual(["set global", "set global var"], ExtensionsGenerator("set \t\t global\t "))}].