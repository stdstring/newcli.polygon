%% @author stdstring

-module(string_utils_tests).

-include_lib("eunit/include/eunit.hrl").

%% ====================================================================
%% Test functions
%% ====================================================================

format_test_() ->
    [{"use single format",
      ?_assertEqual("i = 3", string_utils:format("i = ~p", [3]))},
     {"use several formats",
      ?_assertEqual("i = 3 + 4", string_utils:format("i = ~p + ~p", [3, 4]))}].

get_common_prefix_test_() ->
    [{"common prefix for empty string list",
      ?_assertEqual("", string_utils:get_common_prefix([]))},
    {"common prefix for single string list",
      ?_assertEqual("abcd", string_utils:get_common_prefix(["abcd"]))},
    {"common prefix for strings with common prefix",
      ?_assertEqual("abc", string_utils:get_common_prefix(["abcd", "abcef", "abc"]))},
    {"common prefix for strings without common prefix",
      ?_assertEqual("", string_utils:get_common_prefix(["abcd", "efgh", "ig"]))},
    {"common prefix for strings with empty string",
      ?_assertEqual("", string_utils:get_common_prefix(["abcd", "", "abc"]))},
    {"common prefix for string pair with common prefix",
      ?_assertEqual("abc", string_utils:get_common_prefix("abcd", "abcefg"))},
    {"common prefix for string pair without common prefix",
      ?_assertEqual("", string_utils:get_common_prefix("abcd", "efg"))},
    {"common prefix for string pair with empty string",
      ?_assertEqual("", string_utils:get_common_prefix("abcd", ""))}].

%% ====================================================================
%% Internal functions
%% ====================================================================