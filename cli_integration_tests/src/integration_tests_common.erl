%% @author std-string

-module(integration_tests_common).

-include_lib("eunit/include/eunit.hrl").

-include("integration_tests_defs.hrl").

-define(CRASH_DUMP_FILE, "erl_crash.dump").
-define(CRASH_DUMP_FILES, ["./" ++ ?CRASH_DUMP_FILE, "./backend_ebin/" ++ ?CRASH_DUMP_FILE, "./frontend_ebin/" ++ ?CRASH_DUMP_FILE]).

%% ====================================================================
%% API functions
%% ====================================================================

-export([create_tests_entry/1, check_normal_execution/0, clear_abnormal_execution/0, process/3]).

-spec create_tests_entry(Source :: {Description :: string(), Input :: [string()], Output :: [string()]}) ->
    {'foreach', fun(() -> #integration_test_state{}), fun((#integration_test_state{}) -> 'ok'), [fun((#integration_test_state{}) -> 'ok')]}.
create_tests_entry(Source) ->
    MapFun = fun({Description, Input, Output}) -> create_tests_instantiator(Description, Input, Output) end,
    InstantiatorList = lists:map(MapFun, Source),
    {foreach, fun integration_tests_manager:setup/0, fun integration_tests_manager:cleanup/1, InstantiatorList}.

-spec process(Input :: [string()], ExpectedOutput :: [string()], State :: #integration_test_state{}) -> 'ok' | no_return().
process(Input, ExpectedOutput, #integration_test_state{terminal_cmd = TerminalCmd}) ->
    InputData = string:join(Input, "\n") ++ "\n",
    ?assertEqual(ok, file:write_file(?INPUT_DATA, InputData)),
    OutputData = os:cmd(TerminalCmd),
    OutputDataParts = string:tokens(OutputData, "\n"),
    ?assertEqual(length(ExpectedOutput), length(OutputDataParts)),
    ActualOutput = lists:sublist(OutputDataParts, length(ExpectedOutput)),
    ?assertEqual(ExpectedOutput, ActualOutput),
    check_normal_execution(),
    ok.

-spec check_normal_execution() -> 'ok'.
check_normal_execution() ->
    lists:foreach(fun(Filename) -> ?assertNot(filelib:is_regular(Filename)) end, ?CRASH_DUMP_FILES),
    ok.

-spec clear_abnormal_execution() -> 'ok'.
clear_abnormal_execution() ->
    lists:foreach(fun(Filename) -> file:delete(Filename) end, ?CRASH_DUMP_FILES),
    ok.

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec create_tests_instantiator(Description :: string(), Input :: [string()], Output :: [string()]) ->
    fun((#integration_test_state{}) -> 'ok').
create_tests_instantiator(Description, Input, Output) ->
    fun(State) -> [{Description, fun() -> process(Input, Output, State) end}] end.