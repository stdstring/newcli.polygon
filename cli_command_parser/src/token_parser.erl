%% @author std-string

-module(token_parser).

-export([parse/3]).

-include("common_defs.hrl").
-include("lexical_defs.hrl")

%% ====================================================================
%% API functions
%% ====================================================================

-spec parse(ParserState :: #token_parser_state{}, Char :: byte(), ParserConfig :: #token_parser_config{}) ->
    {'true', #token_parser_result{}} | 'false'.
parse(ParserState, Char, ParserConfig) ->
    CurrentState = ParserState#token_parser_state.current_state,
    TransitionTable = ParserConfig#token_parser_config.transitions,
    case find_transition(CurrentState, Char, TransitionTable) of
        {true, ToState, Appender} ->
            CurrentBuffer = ParserState#token_parser_state.recognized_buffer,
            NewBuffer = Appender(Char, CurrentBuffer),
            NewState = #token_parser_state{current_state = ToState, recognized_buffer = NewBuffer},
            FinalStates = ParserConfig#token_parser_config.final_states,
            TokenFactory = ParserConfig#token_parser_config.token_factory,
            Token = create_token(NewState, FinalStates, TokenFactory),
            {true, #token_parser_result{token = Token, state = NewState}};
        false -> false
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec find_transition(CurrentState :: atom(), Char :: byte(), TransitionTable :: [#transition{}]) ->
    {'true', ToState :: atom()} | 'false'.
find_transition(CurrentState, Char, TransitionTable) ->
    FilterFun = fun(#transition{from_state = FromState, char_predicate = Predicate}) ->
        (FromState == CurrentState) and Predicate(Char)
    end,
    case lists:filter(FilterFun, TransitionTable) of
        [#transition{to_state = ToState, char_appender = Appender}] -> {true, ToState, Appender};
        _Other -> false
    end.

create_token(ParserState, FinalStates, TokenBuilder) ->
    State = ParserState#token_parser_state.current_state,
    case lists:member(State, FinalStates) of
        true -> TokenBuilder(ParserState);
        false -> undefined
    end.