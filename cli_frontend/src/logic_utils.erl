%% @author std-string

-module(logic_utils).

%% ====================================================================
%% API functions
%% ====================================================================

-export([ternary_op/3]).

-spec ternary_op(Condition :: boolean(), OperandTrue :: term(), OperandFalse :: term()) -> boolean().
ternary_op(Condition, OperandTrue, OperandFalse) ->
    case Condition of
        true -> OperandTrue;
        false -> OperandFalse
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

