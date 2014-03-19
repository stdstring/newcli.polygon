%% @author std-string

-module(logic_utils).

%% ====================================================================
%% API functions
%% ====================================================================

-export([ternary_op/3]).

-spec ternary_op(Condition :: fun((term(), term()) -> boolean()), Operand1 :: term(), Operand2 :: term()) -> boolean().
ternary_op(Condition, Operand1, Operand2) ->
    case Condition(Operand1, Operand2) of
        true -> Operand1;
        false -> Operand2
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

