%% @author std-string

-module(module_name_generator_tests).

-include_lib("eunit/include/eunit.hrl").

%% ====================================================================
%% Test functions
%% ====================================================================

generate_test() ->
    ?assertEqual(test_module_192_168_0_1_44332, module_name_generator:generate(test_module, {{192, 168, 0, 1}, 44332})).

%% ====================================================================
%% Internal functions
%% ====================================================================