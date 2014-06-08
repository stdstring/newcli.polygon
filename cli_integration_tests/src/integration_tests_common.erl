%% @author std-string

-module(integration_tests_common).

-include_lib("eunit/include/eunit.hrl").

-include("integration_tests_defs.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

-export([process/3]).

process(Input, ExpectedOutput, #integration_test_state{frontend_cmd = FrontendCmd}) ->
    ?assertEqual(ok, file:write_file(?INPUT_DATA, Input)),
    ActualOutput = os:cmd(FrontendCmd),
    ?assertEqual(ExpectedOutput, ActualOutput).