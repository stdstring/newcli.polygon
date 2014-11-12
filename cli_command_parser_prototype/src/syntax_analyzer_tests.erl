%% @author std-string

-module(syntax_analyzer_tests).

-include("common_defs.hrl").
-include("command_function_defs.hrl").

-include_lib("eunit/include/eunit.hrl").

-define(COMMAND, #nonterminal{name = command}).
-define(ARGS, #nonterminal{name = args}).
-define(WORD_TOKEN, #token{type = word, value = undefined}).
-define(STRING_TOKEN, #token{type = string, value = undefined}).
-define(END_TOKEN, #token{type = 'end', value = ''}).
-define(WORD_TERM, #terminal{type = word, value = undefined}).
-define(STRING_TERM, #terminal{type = string, value = undefined}).
-define(END_TERM, #terminal{type = 'end', value = ''}).

-define(COMMAND_ACTION, fun(NameTable, ProcessState, Token) -> command_action(NameTable, ProcessState, Token) end).
-define(ARGS_ACTION, fun(NameTable, ProcessState, Token) -> args_action(NameTable, ProcessState, Token) end).

-define(EXEC_CONTEXT_MODULE, command_exec_context).
-define(EXEC_CONTEXT_FUNCTION, execute).

%% ====================================================================
%% Test functions
%% ====================================================================

%%syntax_analyzer_process_test_() ->
%%    SyntaxTable = create_syntax_table(),
%%    [].

simple_syntax_analysis_test() ->
    SyntaxTable = create_syntax_table(),
    NameTable = create_name_table(),
    GlobalState = #global_state{syntax_table = SyntaxTable, name_table = NameTable},
    io:format(user, "~nparse 'ping 192.168.0.1' :~n", []),
    {true, _} = syntax_analyzer:process([#token{type = word, value = "ping"}, #token{type = word, value = "192.168.0.1"}, ?END_TOKEN], ?COMMAND, GlobalState),
    io:format(user, "~nparse 'exit' :~n", []),
    {true, _} = syntax_analyzer:process([#token{type = word, value = "exit"}, ?END_TOKEN], ?COMMAND, GlobalState),
    io:format(user, "~nparse 'call \"iddqd idkfa\"' :~n", []),
    {true, _} = syntax_analyzer:process([#token{type = word, value = "call"}, #token{type = string, value = "iddqd idkfa"}, ?END_TOKEN], ?COMMAND, GlobalState),
    io:format(user, "~ntry parse '\"iddqd idkfa\"' :~n", []),
    {false, bad_token} = syntax_analyzer:process([#token{type = string, value = "iddqd idkfa"}, ?END_TOKEN], ?COMMAND, GlobalState),
    io:format(user, "~nparse 'call 666' with unknown token :~n", []),
    {false, bad_token} = syntax_analyzer:process([#token{type = word, value = "call"}, #token{type = integer, value = 666}, ?END_TOKEN], ?COMMAND, GlobalState).

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
    #process_state{current_frame = CommandFrame}.

args_action(_NameTable, #process_state{current_frame = #command_frame{items = Items}}, #token{type = word, value = Word}) ->
    NewCommandFrame = #command_frame{items = [#frame_item{type = word, value = Word}] ++ Items},
    #process_state{current_frame = NewCommandFrame};
args_action(_NameTable, #process_state{current_frame = #command_frame{items = Items}}, #token{type = string, value = String}) ->
    NewCommandFrame = #command_frame{items = [#frame_item{type = string, value = String}] ++ Items},
    #process_state{current_frame = NewCommandFrame};
args_action(NameTable, #process_state{current_frame = #command_frame{items = ReversedItems}}, ?END_TOKEN) ->
    Items = lists:reverse(ReversedItems),
    Result = frame_item_search:search_best(Items, NameTable),
    {true, CommandModule, CommandFunction, CommandArgs} = Result,
    {ok, ?EXEC_CONTEXT_MODULE, _Binary} = code_generator:generate(?EXEC_CONTEXT_MODULE, ?EXEC_CONTEXT_FUNCTION, {CommandModule, CommandFunction, CommandArgs}),
    #process_state{current_frame = undefined}.