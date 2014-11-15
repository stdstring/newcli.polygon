%% @author std-string

-module(token_parser).

-export([parse/3]).

-include("common_defs.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

%%-spec parse(ParserState :: atom(), Char :: byte(), ParserConfig :: #token_parser_config{}) ->
%%    {'true', #token_parser_result{}} | 'false'.
%%parse(ParserState, Char, #token_parser_config{transitions = TransitionTable, final_states = FinalStates}) ->
%%    case find_transition(ParserState, Char, TransitionTable) of
%%        {true, ToState, Appender} ->
%%            IsFinal = lists:member(ToState, FinalStates),
%%            {true, #token_parser_result{to_state = ToState, is_final = IsFinal}};
%%        false -> false
%%    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec find_transition(CurrentState :: atom(), Char :: byte(), TransitionTable :: [#transition{}]) ->
    {'true', ToState :: atom()} | 'false'.
find_transition(CurrentState, Char, TransitionTable) ->
    FilterFun = fun(#transition{from_state = FromState, chars = Chars}) ->
        FromState == CurrentState and lists:member(Char, Chars)
    end,
    case lists:filter(FilterFun, TransitionTable) of
        [#transition{to_state = ToState, char_appender = Appender}] -> {true, ToState, Appender};
        _Other -> false
    end.