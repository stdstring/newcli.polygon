%% @author std-string

-module(logic_utils).

%% ====================================================================
%% API functions
%% ====================================================================

-export([ternary_op/3, ternary_lazy_op/3]).

-spec ternary_op(Condition :: boolean(), OperandTrue :: term(), OperandFalse :: term()) -> boolean().
ternary_op(Condition, OperandTrue, OperandFalse) ->
    case Condition of
        true -> OperandTrue;
        false -> OperandFalse
    end.

-spec ternary_lazy_op(Condition :: boolean(), OperandTrue :: fun(() -> term()), OperandFalse :: fun(() -> term())) -> boolean().
ternary_lazy_op(Condition, OperandTrue, OperandFalse) ->
    case Condition of
        true -> OperandTrue();
        false -> OperandFalse()
    end.


%% ====================================================================
%% Internal functions
%% ====================================================================

