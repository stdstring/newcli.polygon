%% @author stdstring

-module(logic_utils_tests).

-include_lib("eunit/include/eunit.hrl").

-define(LAZY(Arg), fun() -> Arg end).

%% ====================================================================
%% Test functions
%% ====================================================================

ternary_op_test_() ->
    [{"use true operand", ?_assertEqual(true, logic_utils:ternary_op(true, true, false))},
     {"use false operand", ?_assertEqual(false, logic_utils:ternary_op(false, true, false))}].

ternary_lazy_op_test_() ->
    [{"use true operand", ?_assertEqual(true, logic_utils:ternary_lazy_op(true, ?LAZY(true), ?LAZY(error(some_error))))},
     {"use false operand", ?_assertEqual(false, logic_utils:ternary_lazy_op(false, ?LAZY(error(some_error)), ?LAZY(false)))}].