%% @author std-string

-module(syntax_analyzer).

-export([process/3]).

-include("frame_defs.hrl").
-include("lexical_defs.hrl").
-include("name_search_defs.hrl").
-include("syntax_defs.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

-spec process(TokenList :: [#token{}], StartSymbol :: #nonterminal{}, Config :: #syntax_analyzer_config{}) ->
    {'true', Result :: term()} | {'false', Reason :: term()}.
process(TokenList, StartSymbol, Config) ->
    process_impl(TokenList, [StartSymbol], Config, #syntax_process_state{}).

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec process_impl(TokenList :: [#token{}],
                   ProcessStack :: syntax_process_stack(),
                   Config :: #syntax_analyzer_config{},
                   ProcessState :: #syntax_process_state{}) ->
    {'true', Result :: term()} | {'false', Reason :: term()}.
process_impl([], [], _Config, #syntax_process_state{result = Result}) -> {true, Result};
process_impl([], _ProcessStack, _Config, _ProcessState) -> {false, bad_token};
process_impl(TokenList, ProcessStack, Config, ProcessState) ->
    case process_token(TokenList, ProcessStack, Config, ProcessState) of
        {NewTokenList, NewProcessStack, NewProcessState} -> process_impl(NewTokenList, NewProcessStack, Config, NewProcessState);
        {false, Reason} -> {false, Reason}
    end.

-spec process_token(TokenList :: [#token{}],
                    ProcessStack :: syntax_process_stack(),
                    Config :: #syntax_analyzer_config{},
                    ProcessState :: #syntax_process_state{}) ->
    {TokenList :: [#token{}], ProcessStack :: syntax_process_stack(), ProcessState :: #syntax_process_state{}} | {'false', Reason :: term()}.
process_token([Token | TokenListRest], [#terminal{type = Type, value = Value} | ProcessStackRest], _Config, ProcessState) ->
    case check_terminal(Token, #terminal{type = Type, value = Value}) of
        true ->
            {TokenListRest, ProcessStackRest, ProcessState};
        false -> {false, bad_token}
    end;
process_token([Token | _] = TokenList, [#nonterminal{name = Name} | ProcessStackRest], Config, ProcessState) ->
    SyntaxTable = Config#syntax_analyzer_config.syntax_table,
    NameTable = Config#syntax_analyzer_config.name_table,
    case find_production(#nonterminal{name = Name}, Token, SyntaxTable) of
        {ok, Production} ->
            case process_production(Token, Production, ProcessStackRest, NameTable, ProcessState) of
                {true, NewProcessStack, NewProcessState} -> {TokenList, NewProcessStack, NewProcessState};
                {false, Reason} -> {false, Reason}
            end;
        not_found -> {false, bad_token}
    end.

-spec check_terminal(Token :: #token{}, Terminal :: #terminal{}) -> boolean().
check_terminal(#token{type = Type}, #terminal{type = Type, value = undefined}) -> true;
check_terminal(#token{type = Type, value = Value}, #terminal{type = Type, value = Value}) -> true;
check_terminal(#token{}, #terminal{}) -> false.

-spec find_production(Nonterminal :: #nonterminal{},
                      Token :: #token{},
                      SyntaxTable ::syntax_table()) ->
    {'ok', Value :: term()} | term().
find_production(Nonterminal, Token, SyntaxTable) ->
    case dict:find({Nonterminal, Token}, SyntaxTable) of
        {ok, Value} -> {ok, Value};
        error ->
            case dict:find({Nonterminal, Token#token{value = undefined}}, SyntaxTable) of
                {ok, Value} -> {ok, Value};
                error -> not_found
            end
    end.

-spec process_production(Token :: #token{},
                         {Production :: syntax_production(), Action :: syntax_production_action()},
                         ProcessStack :: syntax_process_stack(),
                         NameTable :: name_search_table(),
                         ProcessState :: #syntax_process_state{}) ->
    {'true', ProcessStack :: syntax_process_stack(), ProcessState :: #syntax_process_state{}} | {'false', Reason :: term()}.
process_production(Token, {Production, Action}, ProcessStack, NameTable, ProcessState) ->
    case Action(NameTable, ProcessState, Token) of
        {true, NewProcessState} ->
            NewProcessStack = Production ++ ProcessStack,
            {true, NewProcessStack, NewProcessState};
        {false, Reason} -> {false, Reason}
    end.