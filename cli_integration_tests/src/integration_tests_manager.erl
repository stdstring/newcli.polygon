%% @author std-string

-module(integration_tests_manager).

-include_lib("eunit/include/eunit.hrl").

-export([setup/0, cleanup/1]).

-include("integration_tests_defs.hrl").

-define(INPUT_DATA, "/tmp/input").

%% ====================================================================
%% API functions
%% ====================================================================

-spec setup() -> #integration_test_state{}.
setup() ->
    {ok, CurrentDir} = file:get_cwd(),
    Service = integration_tests_common:start_cli_service(),
    TerminalCmd = filename:join([CurrentDir, "cli_terminal_bin", "cli_terminal"]) ++ " --config=cli_terminal_data/cli_terminal.conf < " ++ ?INPUT_DATA,
    #integration_test_state{service = Service, terminal_cmd = TerminalCmd}.

-spec cleanup(State :: #integration_test_state{}) -> 'ok'.
cleanup(#integration_test_state{service = Service}) ->
    integration_tests_common:stop_cli_service(Service).

%% ====================================================================
%% Internal functions
%% ====================================================================