%% @author stdstring

-module(string_data_utils_tests).

-include_lib("eunit/include/eunit.hrl").

%% ====================================================================
%% Test functions
%% ====================================================================

add_trailing_line_feed_test_() ->
    [{"empty line", ?_assertEqual("\n", string_data_utils:add_trailing_line_feed(""))},
     {"line with single line feed", ?_assertEqual("\n", string_data_utils:add_trailing_line_feed("\n"))},
     {"line without line feed", ?_assertEqual("iddqd idkfa\n", string_data_utils:add_trailing_line_feed("iddqd idkfa"))},
     {"line with line feed", ?_assertEqual("iddqd idkfa\n", string_data_utils:add_trailing_line_feed("iddqd idkfa\n"))},
     {"line with multiple line feeds", ?_assertEqual("iddqd idkfa\n\n", string_data_utils:add_trailing_line_feed("iddqd idkfa\n\n"))}].

remove_trailing_line_feed_test_() ->
    [{"empty line", ?_assertEqual("", string_data_utils:remove_trailing_line_feed(""))},
     {"line with single line feed", ?_assertEqual("", string_data_utils:remove_trailing_line_feed(""))},
     {"line without line feed", ?_assertEqual("iddqd idkfa", string_data_utils:remove_trailing_line_feed("iddqd idkfa"))},
     {"line with line feed", ?_assertEqual("iddqd idkfa", string_data_utils:remove_trailing_line_feed("iddqd idkfa\n"))},
     {"line with multiple line feeds", ?_assertEqual("iddqd idkfa\n", string_data_utils:remove_trailing_line_feed("iddqd idkfa\n\n"))}].

remove_all_trailing_line_feeds_test_() ->
    [{"empty line", ?_assertEqual("", string_data_utils:remove_all_trailing_line_feeds(""))},
     {"line with single line feed", ?_assertEqual("", string_data_utils:remove_all_trailing_line_feeds(""))},
     {"line without line feed", ?_assertEqual("iddqd idkfa", string_data_utils:remove_all_trailing_line_feeds("iddqd idkfa"))},
     {"line with line feed", ?_assertEqual("iddqd idkfa", string_data_utils:remove_all_trailing_line_feeds("iddqd idkfa\n"))},
     {"line with multiple line feeds", ?_assertEqual("iddqd idkfa", string_data_utils:remove_all_trailing_line_feeds("iddqd idkfa\n\n"))}].