%% @author std-string

-module(syntax_analyzer_tests).

-include("common_defs.hrl").
-include("command_function_defs.hrl").
-include("token_defs.hrl").

-include_lib("eunit/include/eunit.hrl").

-define(COMMAND, #nonterminal{name = command}).
-define(ARGS, #nonterminal{name = args}).

-define(WORD_TERM, #terminal{type = word, value = undefined}).
-define(STRING_TERM, #terminal{type = string, value = undefined}).
-define(END_TERM, #terminal{type = 'end', value = ""}).

-define(WORD_TOKEN, ?WORD(undefined)).
-define(STRING_TOKEN, ?STRING(undefined)).

-define(COMMAND_ACTION, fun(NameTable, ProcessState, Token) -> command_action(NameTable, ProcessState, Token) end).
-define(ARGS_ACTION, fun(NameTable, ProcessState, Token) -> args_action(NameTable, ProcessState, Token) end).

-define(EXEC_CONTEXT_MODULE, command_exec_context).
-define(EXEC_CONTEXT_FUNCTION, execute).

%% ====================================================================
%% Test functions
%% ====================================================================

syntax_analyzer_process_test_() ->
    SyntaxTable = create_syntax_table(),
    NameTable = create_name_table(),
    GlobalState = #global_state{syntax_table = SyntaxTable, name_table = NameTable},
    [{"parse 'ping 192.168.0.1'", ?_assertEqual("ping \"192.168.0.1\"", success_execution([?WORD("ping"), ?WORD("192.168.0.1"), ?END_TOKEN], GlobalState))},
     {"parse 'p 192.168.0.1'", ?_assertEqual("ping \"192.168.0.1\"", success_execution([?WORD("p"), ?WORD("192.168.0.1"), ?END_TOKEN], GlobalState))},
     {"parse 'interface \"some interface\"'", ?_assertEqual("interface \"some interface\"", success_execution([?WORD("interface"), ?STRING("some interface"), ?END_TOKEN], GlobalState))},
     {"parse 'exit'", ?_assertEqual("exit", success_execution([?WORD("exit"), ?END_TOKEN], GlobalState))},
     {"try parse 'call \"iddqd idkfa\"'", ?_assertEqual(command_not_found, fail_execution([?WORD("CALL"), ?STRING("iddqd idkfa"), ?END_TOKEN], GlobalState))},
     {"try parse '\"iddqd idkfa\"'", ?_assertEqual(bad_token, fail_execution([?STRING("iddqd idkfa"), ?END_TOKEN], GlobalState))},
     {"try parse 'call 666' with unknown token", ?_assertEqual(bad_token, fail_execution([?WORD("call"), ?TOKEN(integer, 666), ?END_TOKEN], GlobalState))}].

%% ====================================================================
%% Internal functions
%% ====================================================================

create_syntax_table() ->
    Table =[{{?COMMAND, ?WORD_TOKEN}, {[?WORD_TERM, ?ARGS], ?COMMAND_ACTION}},
            {{?ARGS, ?WORD_TOKEN}, {[?WORD_TERM, ?ARGS], ?ARGS_ACTION}},
            {{?ARGS, ?STRING_TOKEN}, {[?STRING_TERM, ?ARGS], ?ARGS_ACTION}},
            {{?ARGS, ?END_TOKEN}, {[?END_TERM], ?ARGS_ACTION}}],
    dict:from_list(Table).

create_name_table() ->
    [{[{"ping", 1}], ?MODULE_NAME, ?PING_FUNCTION},
     {[{"configure", 1}, {"terminal", 1}], ?MODULE_NAME, ?CONF_TERM_FUNCTION},
     {[{"login", 4}], ?MODULE_NAME, ?LOGIN_FUNCTION},
     {[{"logout", 4}], ?MODULE_NAME, ?LOGOUT_FUNCTION},
     {[{"interface", 1}], ?MODULE_NAME, ?INTERFACE_FUNCTION},
     {[{"interface", 1}, {"range", 1}], ?MODULE_NAME, ?IFRANGE_FUNCTION},
     {[{"vlan", 1}], ?MODULE_NAME, ?VLAN_FUNCTION},
     {[{"no", 2}, {"vlan", 1}], ?MODULE_NAME, ?NOVLAN_FUNCTION},
     {[{"switchport", 2}, {"access", 1}, {"vlan", 1}], ?MODULE_NAME, ?SWACCESS_VLAN_FUNCTION},
     {[{"no", 2}, {"switchport", 1}, {"access", 1}, {"vlan", 1}], ?MODULE_NAME, ?NOSWACCESS_VLAN_FUNCTION},
     {[{"name", 2}], ?MODULE_NAME, ?NAME_FUNCTION},
     {[{"no", 2}, {"name", 1}], ?MODULE_NAME, ?NONAME_FUNCTION},
     {[{"end", 2}], ?MODULE_NAME, ?END_FUNCTION},
     {[{"exit", 2}], ?MODULE_NAME, ?EXIT_FUNCTION},
     {[{"show", 2}, {"vlan", 1}], ?MODULE_NAME, ?SHOW_VLAN_FUNCTION}].

command_action(_NameTable, #process_state{current_frame = undefined}, #token{type = word, value = Word}) ->
    CommandFrame = #command_frame{items = [#frame_item{type = word, value = Word}]},
    {true, #process_state{current_frame = CommandFrame}}.

args_action(_NameTable, #process_state{current_frame = #command_frame{items = Items}}, #token{type = word, value = Word}) ->
    NewCommandFrame = #command_frame{items = [#frame_item{type = word, value = Word}] ++ Items},
    {true, #process_state{current_frame = NewCommandFrame}};
args_action(_NameTable, #process_state{current_frame = #command_frame{items = Items}}, #token{type = string, value = String}) ->
    NewCommandFrame = #command_frame{items = [#frame_item{type = string, value = String}] ++ Items},
    {true, #process_state{current_frame = NewCommandFrame}};
args_action(NameTable, #process_state{current_frame = #command_frame{items = Items}}, ?END_TOKEN) ->
    case generate_code(lists:reverse(Items), NameTable) of
        {true, Binary} -> {true, #process_state{current_frame = undefined, binary_code = Binary}};
        false -> {false, command_not_found}
    end.

generate_code(Items, NameTable) ->
    case frame_item_search:search_best(Items, NameTable) of
        {true, CommandModule, CommandFunction, CommandArgs} ->
            {ok, ?EXEC_CONTEXT_MODULE, Binary} = code_generator:generate(?EXEC_CONTEXT_MODULE, ?EXEC_CONTEXT_FUNCTION, {CommandModule, CommandFunction, CommandArgs}),
            {true, Binary};
        false -> false
    end.

success_execution(TokenList, GlobalState) ->
    {true, Binary} = syntax_analyzer:process(TokenList, ?COMMAND, GlobalState),
    {module, ?EXEC_CONTEXT_MODULE} = code:load_binary(?EXEC_CONTEXT_MODULE, [], Binary),
    ?EXEC_CONTEXT_MODULE:?EXEC_CONTEXT_FUNCTION().

fail_execution(TokenList, GlobalState) ->
    {false, Reason} = syntax_analyzer:process(TokenList, ?COMMAND, GlobalState),
    Reason.