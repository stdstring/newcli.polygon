%% @author std-string

-module(integration_tests_common).

-include_lib("eunit/include/eunit.hrl").

-include("integration_tests_defs.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

-export([create_test_entry/3, process/3]).

create_test_entry(Input, ExpectedOutput, TestDecription) ->
    {foreach,
    fun integration_tests_manager:setup/0,
    fun integration_tests_manager:cleanup/1,
    [fun(State) -> [{TestDecription, fun() -> process(Input, ExpectedOutput, State) end}] end]}.

process(Input, ExpectedOutput, #integration_test_state{frontend_cmd = FrontendCmd}) ->
    ?assertEqual(ok, file:write_file(?INPUT_DATA, Input)),
    ActualOutput = os:cmd(FrontendCmd),
    ?debugFmt("ActualOutput: ~s~n", [ActualOutput]),
    ?assertEqual(ExpectedOutput, ActualOutput).