-module(command_parser_tests).

-include_lib("eunit/include/eunit.hrl").

-include("common_defs.hrl").
-include("syntax_analyzer_defs.hrl").
-include("token_defs.hrl").

%% ====================================================================
%% Test functions
%% ====================================================================

parse_command_test_() ->
    LexConfig = lex_analyzer_config:create_config(),
    NameConfig = name_search_config:create_config(),
    SyntaxConfig = syntax_analyzer_config:create_config(),
    [success_execution("process 'ping XXX'", "ping XXX", "ping \"XXX\"", LexConfig, NameConfig, SyntaxConfig),
     success_execution("process 'p XXX'", "p XXX", "ping \"XXX\"", LexConfig, NameConfig, SyntaxConfig),
     success_execution("process 'c t'", "c t", "configure terminal", LexConfig, NameConfig, SyntaxConfig),
     fail_execution("process 'ping +'", "ping +", unsuitable_char, LexConfig, NameConfig, SyntaxConfig),
     fail_execution("process 'ping +XXX'", "ping +", unsuitable_char, LexConfig, NameConfig, SyntaxConfig),
     fail_execution("process 'ping \"'", "ping \"", bad_input, LexConfig, NameConfig, SyntaxConfig),
     fail_execution("process 'ping \"XXX'", "ping \"XXX", bad_input, LexConfig, NameConfig, SyntaxConfig),
     fail_execution("process 'route XXX'", "route XXX", command_not_found, LexConfig, NameConfig, SyntaxConfig),
     fail_execution("process 'n YYY'", "n YYY", command_not_found, LexConfig, NameConfig, SyntaxConfig),
     fail_execution("process '\"ping\"'", "\"ping\"", bad_token, LexConfig, NameConfig, SyntaxConfig)].

%% ====================================================================
%% Internal functions
%% ====================================================================

success_execution(Description, Source, Expected, LexConfig, NameConfig, SyntaxConfig) ->
    {Description, ?_assertEqual({true, Expected}, process(Source, LexConfig, NameConfig, SyntaxConfig))}.

fail_execution(Description, Source, Reason, LexConfig, NameConfig, SyntaxConfig) ->
    {Description, ?_assertEqual({false, Reason}, process(Source, LexConfig, NameConfig, SyntaxConfig))}.

process(Source, LexConfig, NameConfig, SyntaxConfig) ->
    case lex_analyzer:parse(Source, LexConfig, true) of
        {true, TokenList} ->
            GlobalState = #global_state{syntax_table = SyntaxConfig, name_table = NameConfig},
            case syntax_analyzer:process(TokenList, ?COMMAND, GlobalState) of
                {true, Binary} ->
                    {module, ?EXEC_CONTEXT_MODULE} = code:load_binary(?EXEC_CONTEXT_MODULE, [], Binary),
                    Result = ?EXEC_CONTEXT_MODULE:?EXEC_CONTEXT_FUNCTION(),
                    {true, Result};
                {false, Reason} -> {false, Reason}
            end;
        {false, Reason} -> {false, Reason}
    end.
