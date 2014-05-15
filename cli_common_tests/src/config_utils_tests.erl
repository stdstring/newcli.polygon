%% @author stdstring

-module(config_utils_tests).

-include_lib("eunit/include/eunit.hrl").

%% ====================================================================
%% Test functions
%% ====================================================================

get_config_test_() ->
    [{"single config, key-value pair, key first", ?_assertEqual(some_value, config_utils:get_config([{some_key, some_value}], some_key, 1, bad_config))},
     {"single config, key-value pair, key last", ?_assertEqual(some_value, config_utils:get_config([{some_value, some_key}], some_key, 2, bad_config))},
     {"single config, tuple, key first", ?_assertEqual({value1, value2}, config_utils:get_config([{some_key, value1, value2}], some_key, 1, bad_config))},
     {"single config, tuple, key middle", ?_assertEqual({value1, value2}, config_utils:get_config([{value1, some_key, value2}], some_key, 2, bad_config))},
     {"single config, tuple, key last", ?_assertEqual({value1, value2}, config_utils:get_config([{value1, value2, some_key}], some_key, 3, bad_config))},
     {"multiple config, key-value pair, key first", ?_assertEqual(some_value, config_utils:get_config([{key1, value1}, {some_key, some_value}], some_key, 1, bad_config))},
     {"multiple config, key-value pair, key last", ?_assertEqual(some_value, config_utils:get_config([{key1, value1}, {some_value, some_key}], some_key, 2, bad_config))},
     {"multiple config, tuple, key first", ?_assertEqual({value1, value2}, config_utils:get_config([{key1, value1}, {some_key, value1, value2}], some_key, 1, bad_config))},
     {"multiple config, tuple, key middle", ?_assertEqual({value1, value2}, config_utils:get_config([{key1, value1}, {value1, some_key, value2}], some_key, 2, bad_config))},
     {"multiple config, tuple, key last", ?_assertEqual({value1, value2}, config_utils:get_config([{key1, value1}, {value1, value2, some_key}], some_key, 3, bad_config))}].

get_config_absent_test_() ->
    [{"single config", ?_assertError(bad_config, config_utils:get_config([{some_key, some_value}], key1, 1, bad_config))},
     {"multiple config", ?_assertError(bad_config, config_utils:get_config([{some_key, some_value}, {other_key, value1, value2}], key1, 1, bad_config))}].