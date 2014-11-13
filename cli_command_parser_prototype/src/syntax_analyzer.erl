%% @author std-string

-module(syntax_analyzer).

-export([process/3]).

-include("common_defs.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

-spec process(TokenList :: [#token{}], StartSymbol :: #nonterminal{}, GlobalState :: #global_state{}) ->
    {'true', ModuleBinary :: binary()} | {'false', Reason :: term()}.
process(TokenList, StartSymbol, GlobalState) ->
    process_impl(TokenList, [StartSymbol], GlobalState, #process_state{}).

%% ====================================================================
%% Internal functions
%% ====================================================================

process_impl([], [], _GlobalState, #process_state{binary_code = Binary}) -> {true, Binary};
process_impl([], _ProcessStack, _GlobalState, _ProcessState) -> {false, bad_token};
process_impl(TokenList, ProcessStack, GlobalState, ProcessState) ->
    %%io:format(user, "ProcessStack : ~p~n", [ProcessStack]),
    case process_token(TokenList, ProcessStack, GlobalState, ProcessState) of
        {NewTokenList, NewProcessStack, NewProcessState} -> process_impl(NewTokenList, NewProcessStack, GlobalState, NewProcessState);
        {false, Reason} -> {false, Reason}
    end.

%% SyntaxTable = dict({nonterminal.Name, token.Type, token.Value | undefined} -> {Production, Action} | error)
process_token([Token | TokenListRest], [#terminal{type = Type, value = Value} | ProcessStackRest], _GlobalState, ProcessState) ->
    %%io:format(user, "process terminal (type = ~p, value = ~p)~n", [Type, Value]),
    case check_terminal(Token, #terminal{type = Type, value = Value}) of
        true ->
            %% add processed token to some state
            {TokenListRest, ProcessStackRest, ProcessState};
        false -> bad_token
    end;
process_token([Token | _] = TokenList, [#nonterminal{name = Name} | ProcessStackRest], GlobalState, ProcessState) ->
    %%io:format(user, "process nonterminal (name = ~p)~n", [Name]),
    SyntaxTable = GlobalState#global_state.syntax_table,
    NameTable = GlobalState#global_state.name_table,
    case find_production(#nonterminal{name = Name}, Token, SyntaxTable) of
        {ok, Production} ->
            case process_production(Token, Production, ProcessStackRest, NameTable, ProcessState) of
                {true, NewProcessStack, NewProcessState} -> {TokenList, NewProcessStack, NewProcessState};
                {false, Reason} -> {false, Reason}
            end;
        not_found -> {false, bad_token}
    end.

check_terminal(#token{type = Type}, #terminal{type = Type, value = undefined}) -> true;
check_terminal(#token{type = Type, value = Value}, #terminal{type = Type, value = Value}) -> true;
check_terminal(#token{}, #terminal{}) -> false.

find_production(Nonterminal, Token, SyntaxTable) ->
    case dict:find({Nonterminal, Token}, SyntaxTable) of
        {ok, Value} -> {ok, Value};
        error ->
            case dict:find({Nonterminal, Token#token{value = undefined}}, SyntaxTable) of
                {ok, Value} -> {ok, Value};
                error -> not_found
            end
    end.

process_production(Token, {Production, Action}, ProcessStackRest, NameTable, ProcessState) ->
    case Action(NameTable, ProcessState, Token) of
        {true, NewProcessState} ->
            ProcessStack = Production ++ ProcessStackRest,
            {true, ProcessStack, NewProcessState};
        {false, Reason} -> {false, Reason}
    end.