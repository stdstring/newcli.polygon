%% @author std-string

-module(char_category_tests).

-include_lib("eunit/include/eunit.hrl").

%% ====================================================================
%% Test functions
%% ====================================================================

is_digit_test_() ->
    [{"is 0 digit", ?_assert(char_category:is_digit($0))},
     {"is 5 digit", ?_assert(char_category:is_digit($5))},
     {"is Z digit", ?_assertNot(char_category:is_digit($Z))},
     {"is x digit", ?_assertNot(char_category:is_digit($x))},
     {"is \\t digit", ?_assertNot(char_category:is_digit($\t))},
     {"is ? digit", ?_assertNot(char_category:is_digit($?))},
     {"is * digit", ?_assertNot(char_category:is_digit($*))}].

is_letter_test_() ->
    [{"is 0 letter", ?_assertNot(char_category:is_letter($0))},
     {"is 5 letter", ?_assertNot(char_category:is_letter($5))},
     {"is Z letter", ?_assert(char_category:is_letter($Z))},
     {"is x letter", ?_assert(char_category:is_letter($x))},
     {"is \\t letter", ?_assertNot(char_category:is_letter($\t))},
     {"is ? letter", ?_assertNot(char_category:is_letter($?))},
     {"is * letter", ?_assertNot(char_category:is_letter($*))}].

is_space_test_() ->
    [{"is 0 space", ?_assertNot(char_category:is_space($0))},
     {"is 5 space", ?_assertNot(char_category:is_space($5))},
     {"is Z space", ?_assertNot(char_category:is_space($Z))},
     {"is x space", ?_assertNot(char_category:is_space($x))},
     {"is \\t space", ?_assert(char_category:is_space($\t))},
     {"is ? space", ?_assertNot(char_category:is_space($?))},
     {"is * space", ?_assertNot(char_category:is_space($*))}].

%% ====================================================================
%% Internal functions
%% ====================================================================