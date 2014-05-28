%% @author stdstring

-module(integration_tests).

-include_lib("eunit/include/eunit.hrl").

-record(integration_test_state, {backend = undefined :: 'undefined' | port(), frontend = undefined :: 'undefined' | port()}).

%% ====================================================================
%% Test functions
%% ====================================================================
 
 integration_test_() ->
    [{foreach, fun() -> setup() end, fun(State) -> cleanup(State) end, [fun(State) -> [fun() -> help_test_instance(State) end] end]}].

-spec setup() -> #integration_test_state{}.
setup() ->
    ErlangExecutablePath = os:find_executable("erl"),
    BackendArgs = ["-noshell" "-sname backend_node@polygon-vm" "-s application start cli_backend_application"],
    Backend = external_process:create(ErlangExecutablePath, BackendArgs),
    FrontendArgs = ["-noshell", "-sname frontend_node@polygon-vm", "-run cli_frontend_application main ../frontend_data/frontend.conf"],
    Frontend = external_process:create(ErlangExecutablePath, FrontendArgs),
    #integration_test_state{backend = Backend, frontend = Frontend}.

-spec cleanup(State :: #integration_test_state{}) -> ok.
cleanup(#integration_test_state{backend = Backend, frontend = Frontend}) ->
    port_close(Backend),
    port_close(Frontend),
    ok.

help_test_instance(#integration_test_state{frontend = Frontend}) ->
    external_process:send_data_to_process(Frontend, "i?\n"),
    ?debugFmt("Messages: ~p~n", [external_process:receive_data_from_process()]).