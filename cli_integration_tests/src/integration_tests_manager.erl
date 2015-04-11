%% @author std-string

-module(integration_tests_manager).

-include_lib("eunit/include/eunit.hrl").

-export([setup/0, cleanup/1]).

-include("integration_tests_defs.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

-spec setup() -> #integration_test_state{}.
setup() ->
    %% copy files from cservide_data
    integration_tests_common:prepare_cli_service_data(),
    %% create cli_service
    Service = integration_tests_common:start_cli_service(),
    {ok, CurrentDir} = file:get_cwd(),
    TerminalCmd = filename:join([CurrentDir, "cli_terminal_bin", "cli_terminal"]) ++ ?CLI_TERMINAL_ARGS,
    #integration_test_state{service = Service, terminal_cmd = TerminalCmd}.

-spec cleanup(State :: #integration_test_state{}) -> 'ok'.
cleanup(#integration_test_state{service = Service}) ->
    integration_tests_common:stop_cli_service(Service).

%% ====================================================================
%% Internal functions
%% ====================================================================