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
    %%BackendArgs = " -noshell -sname backend_node@polygon-vm -s application start cli_backend_application >> /tmp/d1",
    %%BackendArgs = " -noshell -sname backend_node -s entry_point start > /tmp/d1 2>&1",
    BackendArgs = " -noshell -sname backend_node -s entry_point start",
    Backend = external_process:create(ErlangExecutablePath ++ BackendArgs, "/home/std-string/work/newcli/cli_integration_tests/backend_ebin/"),
    timer:sleep(20000),
    BackendPingResult = net_adm:ping('backend_node@polygon-vm'),
    ?debugFmt("BackendPingResult: ~p~n", [BackendPingResult]),
    CommandsInfo = gen_server:call({global_input_endpoint, 'backend_node@polygon-vm'}, {commands_info}),
    ?debugFmt("CommandsInfo: ~p~n", [CommandsInfo]),
    %%FrontendArgs = " -noshell -sname frontend_node@polygon-vm -run cli_frontend_application main /home/stdstring/work/newcli/cli_integration_tests/frontend_data/frontend.conf  >> /tmp/d2",
    %%FrontendArgs = " -noshell -sname frontend_node -run cli_frontend_application main /home/std-string/work/newcli/cli_integration_tests/frontend_data/frontend.conf  > /tmp/d2 2>&1",
    FrontendArgs = " -noshell -sname frontend_node -run cli_frontend_application main /home/std-string/work/newcli/cli_integration_tests/frontend_data/frontend.conf",
    Frontend = external_process:create(ErlangExecutablePath ++ FrontendArgs, "/home/std-string/work/newcli/cli_integration_tests/frontend_ebin/"),
    timer:sleep(20000),
    FrontendPingResult = net_adm:ping('frontend_node@polygon-vm'),
    ?debugFmt("FrontendPingResult: ~p~n", [FrontendPingResult]),
    #integration_test_state{backend = Backend, frontend = Frontend}.

-spec cleanup(State :: #integration_test_state{}) -> ok.
cleanup(#integration_test_state{backend = Backend, frontend = Frontend}) ->
    port_close(Backend),
    port_close(Frontend),
    ok.

help_test_instance(#integration_test_state{frontend = Frontend}) ->
    external_process:send_data_to_process(Frontend, "i?\n"),
    external_process:send_data_to_process(Frontend, "interface ?\n"),
    timer:sleep(2000),
    Messages = message_reader:read_all_messages(),
    ?debugFmt("Messages: ~p~n", [Messages]).