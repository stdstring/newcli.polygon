%% @author std-string

-module(integration_tests_common).

-include_lib("eunit/include/eunit.hrl").

-include("integration_tests_defs.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

-export([create_tests_entry/1, process/3]).

-spec create_tests_entry(Source :: {Input :: [string()], Output :: [string()], Description :: string()}) ->
    {'foreach', fun(() -> #integration_test_state{}), fun((#integration_test_state{}) -> 'ok'), [fun((#integration_test_state{}) -> 'ok')]}.
create_tests_entry(Source) ->
    InstantiatorList = lists:map(fun({Input, Output, Description}) -> create_tests_instantiator(Input, Output, Description) end, Source),
    {foreach, fun integration_tests_manager:setup/0, fun integration_tests_manager:cleanup/1, InstantiatorList}.

-spec process(Input :: [string()], ExpectedOutput :: [string()], State :: #integration_test_state{}) -> 'ok' | no_return().
process(Input, ExpectedOutput, #integration_test_state{frontend_cmd = FrontendCmd}) ->
    InputData = string:join(Input, "\n") ++ "\n",
    ?assertEqual(ok, file:write_file(?INPUT_DATA, InputData)),
    OutputData = os:cmd(FrontendCmd),
    OutputDataParts = string:tokens(OutputData, "\n"),
    ?assertEqual(length(ExpectedOutput)+1, length(OutputDataParts)),
    ActualOutput = lists:sublist(OutputDataParts, length(ExpectedOutput)),
    ?debugFmt("ActualOutput: ~p~n", [ActualOutput]),
    ?assertEqual(ExpectedOutput, ActualOutput).

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec create_tests_instantiator(Input :: [string()], Output :: [string()], Description :: string()) -> fun((#integration_test_state{}) -> 'ok').
create_tests_instantiator(Input, Output, Description) ->
    fun(State) -> [{Description, fun() -> process(Input, Output, State) end}] end.