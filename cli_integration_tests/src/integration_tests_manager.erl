%% @author std-string

-module(integration_tests_manager).

-include_lib("eunit/include/eunit.hrl").

-export([setup/0, cleanup/1]).

-record(integration_test_state, {service = undefined :: 'undefined' | port(), terminal_cmd = "" :: string()}).

-define(MAX_LINE_LENGTH, 1000).
-define(SERVICE_NODE, 'cli_service_node@polygon-vm').
-define(SERVICE_PROCESS, cli_terminal_listen_endpoint).
-define(SERVICE_ARGS, " -noshell -sname ~s -eval \"application:start(cli_backend_application)\"").
-define(INPUT_DATA, "/tmp/input").

%% ====================================================================
%% API functions
%% ====================================================================

-spec setup() -> #integration_test_state{}.
setup() ->
    {ok, CurrentDir} = file:get_cwd(),
    ErlangExecutablePath = os:find_executable("erl"),
    ServiceArgs = string_utils:format(?SERVICE_ARGS, ?SERVICE_NODE),
    ServiceDir = filename:join([CurrentDir, "service_ebin"]),
    ServiceSettings = [{line, ?MAX_LINE_LENGTH}, {cd, ServiceDir}, stream, use_stdio, exit_status, stderr_to_stdout],
    Service = open_port({spawn, ErlangExecutablePath ++ ServiceArgs}, ServiceSettings),
    true = wait_process(?SERVICE_NODE, ?SERVICE_PROCESS, 10, 500),
    TerminalCmd = filename:join([CurrentDir, "cli_terminal_bin", "cli_terminal"]) ++ " < " ++ ?INPUT_DATA,
    #integration_test_state{service = Service, terminal_cmd = TerminalCmd}.

-spec cleanup(State :: #integration_test_state{}) -> 'ok'.
cleanup(#integration_test_state{service = Service}) ->
    port_close(Service),
    rpc:call(?SERVICE_NODE, init, stop, []),
    wait_node_exit(?SERVICE_NODE, 10, 500),
    ok.

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec wait_process(Node :: atom(), Process :: atom(), Count :: integer(), WaitTime :: integer()) -> boolean().
wait_process(Node, Process, 0, _WaitTime) ->
    case rpc:call(Node, erlang, whereis, [Process]) of
        {badrpc, nodedown} -> false;
        undefined -> false;
        _Pid -> true
    end;
wait_process(Node, Process, Count, WaitTime) ->
    case rpc:call(Node, erlang, whereis, [Process]) of
        {badrpc, nodedown} ->
            timer:sleep(WaitTime),
            wait_process(Node, Process, Count-1, WaitTime);
        undefined ->
            timer:sleep(WaitTime),
            wait_process(Node, Process, Count-1, WaitTime);
        _Pid -> true
    end.

-spec wait_node_exit(Node :: atom(), Count :: integer(), WaitTime :: integer()) -> boolean().
wait_node_exit(Node, 0, _WaitTime) ->
    case net_adm:ping(Node) of
        pang -> true;
        pong -> false
    end;
wait_node_exit(Node, Count, WaitTime) ->
    case net_adm:ping(Node) of
        pong ->
            timer:sleep(WaitTime),
            wait_node_exit(Node, Count-1, WaitTime);
        pang -> true
    end.