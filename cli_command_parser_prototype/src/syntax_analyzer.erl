%% @author std-string

-module(syntax_analyzer).

-export([process/2]).

-include("common_defs.hrl").

-record(process_state, {token_list = [], process_stack = []}).

%% ====================================================================
%% API functions
%% ====================================================================

%%process(_TokenList, _SyntaxTable, _CommandTable) -> ok.

process(_TokenList, _SyntaxTable) -> ok.

%% ====================================================================
%% Internal functions
%% ====================================================================

%% SyntaxTable = dict({nonterminal.Name, token.Type, token.Value | undefined} -> Production | error)
process_token([Token | TokenListRest], [#terminal{type = Type, value = Value} | ProcessStackRest], SyntaxTable) ->
    case check_terminal(Token, #terminal{type = Type, value = Value}) of
        true ->
            %% add processed token to some state
            #process_state{token_list = TokenListRest, process_stack = ProcessStackRest};
        false -> bad_token
    end;
process_token([Token | TokenListRest] = TokenList, [#nonterminal{name = Name} | ProcessStackRest], SyntaxTable) ->
    case find_production(#nonterminal{name = Name}, Token, SyntaxTable) of
        {ok, Production} ->
            NewProcessStack = process_production(Production, ProcessStackRest),
            #process_state{token_list = TokenListRest, process_stack = NewProcessStack};
        not_found ->
    end,
    ok.

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