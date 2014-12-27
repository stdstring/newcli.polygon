%% @author stdstring

-module(list_utils_tests).

-include_lib("eunit/include/eunit.hrl").

%% ====================================================================
%% Test functions
%% ====================================================================

get_value_by_key_test_() ->
    [{"single config, key-value pair, key first",
      ?_assertEqual(some_value, list_utils:get_value_by_key([{some_key, some_value}], some_key, 1, bad_config))},
     {"single config, key-value pair, key last",
      ?_assertEqual(some_value, list_utils:get_value_by_key([{some_value, some_key}], some_key, 2, bad_config))},
     {"single config, tuple, key first",
      ?_assertEqual({value1, value2}, list_utils:get_value_by_key([{some_key, value1, value2}], some_key, 1, bad_config))},
     {"single config, tuple, key middle",
      ?_assertEqual({value1, value2}, list_utils:get_value_by_key([{value1, some_key, value2}], some_key, 2, bad_config))},
     {"single config, tuple, key last",
      ?_assertEqual({value1, value2}, list_utils:get_value_by_key([{value1, value2, some_key}], some_key, 3, bad_config))},
     {"multiple config, key-value pair, key first",
      ?_assertEqual(some_value, list_utils:get_value_by_key([{key1, value1}, {some_key, some_value}], some_key, 1, bad_config))},
     {"multiple config, key-value pair, key last",
      ?_assertEqual(some_value, list_utils:get_value_by_key([{key1, value1}, {some_value, some_key}], some_key, 2, bad_config))},
     {"multiple config, tuple, key first",
      ?_assertEqual({value1, value2}, list_utils:get_value_by_key([{key1, value1}, {some_key, value1, value2}], some_key, 1, bad_config))},
     {"multiple config, tuple, key middle",
      ?_assertEqual({value1, value2}, list_utils:get_value_by_key([{key1, value1}, {value1, some_key, value2}], some_key, 2, bad_config))},
     {"multiple config, tuple, key last",
      ?_assertEqual({value1, value2}, list_utils:get_value_by_key([{key1, value1}, {value1, value2, some_key}], some_key, 3, bad_config))}].

get_value_by_key_with_default_test_() ->
    [{"single config, key-value pair, key first",
      ?_assertEqual(some_value, list_utils:get_value_by_key_with_default([{some_key, some_value}], some_key, 1, default_value))},
     {"single config, key-value pair, key last",
      ?_assertEqual(some_value, list_utils:get_value_by_key_with_default([{some_value, some_key}], some_key, 2, default_value))},
     {"single config, tuple, key first",
      ?_assertEqual({value1, value2}, list_utils:get_value_by_key_with_default([{some_key, value1, value2}], some_key, 1, default_value))},
     {"single config, tuple, key middle",
      ?_assertEqual({value1, value2}, list_utils:get_value_by_key_with_default([{value1, some_key, value2}], some_key, 2, default_value))},
     {"single config, tuple, key last",
      ?_assertEqual({value1, value2}, list_utils:get_value_by_key_with_default([{value1, value2, some_key}], some_key, 3, default_value))},
     {"multiple config, key-value pair, key first",
      ?_assertEqual(some_value, list_utils:get_value_by_key_with_default([{key1, value1}, {some_key, some_value}], some_key, 1, default_value))},
     {"multiple config, key-value pair, key last",
      ?_assertEqual(some_value, list_utils:get_value_by_key_with_default([{key1, value1}, {some_value, some_key}], some_key, 2, default_value))},
     {"multiple config, tuple, key first",
      ?_assertEqual({value1, value2}, list_utils:get_value_by_key_with_default([{key1, value1}, {some_key, value1, value2}], some_key, 1, default_value))},
     {"multiple config, tuple, key middle",
      ?_assertEqual({value1, value2}, list_utils:get_value_by_key_with_default([{key1, value1}, {value1, some_key, value2}], some_key, 2, default_value))},
     {"multiple config, tuple, key last",
      ?_assertEqual({value1, value2}, list_utils:get_value_by_key_with_default([{key1, value1}, {value1, value2, some_key}], some_key, 3, default_value))}].

get_value_by_key_absent_test_() ->
    [{"single config",
      ?_assertError(bad_config, list_utils:get_value_by_key([{some_key, some_value}], key1, 1, bad_config))},
     {"multiple config",
      ?_assertError(bad_config, list_utils:get_value_by_key([{some_key, some_value}, {other_key, value1, value2}], key1, 1, bad_config))},
     {"single config with default value",
      ?_assertEqual(default_value, list_utils:get_value_by_key_with_default([{some_key, some_value}], key1, 1, default_value))},
     {"multiple config with default value",
      ?_assertEqual(default_value, list_utils:get_value_by_key_with_default([{some_key, some_value}, {other_key, value1, value2}], key1, 1, default_value))}].

%% ====================================================================
%% Internal functions
%% ====================================================================