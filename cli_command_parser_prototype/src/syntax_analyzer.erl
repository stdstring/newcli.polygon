%% @author std-string

-module(syntax_analyzer).

-export([process/3]).

-include("common_defs.hrl").

%%-record(process_result, {token_list = [], process_stack = []}).
%%-record(command_frame_item, {value= undefined, type = undefined}).
%%-record(command_frame, {items = []}).
-record(process_state, {current_frame = undefined}).

%% ====================================================================
%% API functions
%% ====================================================================

process(TokenList, SyntaxTable, StartSymbol) ->
    process_impl(TokenList, [StartSymbol], SyntaxTable, #process_state{}).

%% ====================================================================
%% Internal functions
%% ====================================================================

process_impl([], [], _SyntaxTable, ProcessState) ->
    io:format(user, "current frame : ~p~n", [ProcessState#process_state.current_frame]),
    {ok, []};
process_impl([], _ProcessStack, _SyntaxTable, _ProcessState) -> bad_token;
process_impl(TokenList, ProcessStack, SyntaxTable, ProcessState) ->
    io:format(user, "ProcessStack : ~p~n", [ProcessStack]),
    case process_token(TokenList, ProcessStack, SyntaxTable) of
        {NewTokenList, NewProcessStack} -> process_impl(NewTokenList, NewProcessStack, SyntaxTable, ProcessState);
        bad_token -> bad_token
    end.

%% SyntaxTable = dict({nonterminal.Name, token.Type, token.Value | undefined} -> Production | error)
process_token([Token | TokenListRest], [#terminal{type = Type, value = Value} | ProcessStackRest], _SyntaxTable) ->
    io:format(user, "process terminal (type = ~p, value = ~p)~n", [Type, Value]),
    case check_terminal(Token, #terminal{type = Type, value = Value}) of
        true ->
            %% add processed token to some state
            {TokenListRest, ProcessStackRest};
        false -> bad_token
    end;
process_token([Token | _] = TokenList, [#nonterminal{name = Name} | ProcessStackRest], SyntaxTable) ->
    io:format(user, "process nonterminal (name = ~p)~n", [Name]),
    case find_production(#nonterminal{name = Name}, Token, SyntaxTable) of
        {ok, Production} ->
            NewProcessStack = process_production(Production, ProcessStackRest),
            {TokenList, NewProcessStack};
        not_found -> bad_token
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

process_production(Production, ProcessStackRest) ->
    %% run additional actions
    Production ++ ProcessStackRest.