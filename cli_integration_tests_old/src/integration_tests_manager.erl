%% @author std-string

-module(integration_tests_manager).

-include_lib("eunit/include/eunit.hrl").

-include("integration_tests_defs.hrl").

-define(MAX_LINE_LENGTH, 1000).
-define(BACKEND_ARGS, " -noshell -sname ~s -eval \"application:start(cli_backend_application)\"").
-define(FRONTEND_ARGS, " -noshell -sname ~s -eval \"application:start(cli_frontend_application)\"").

%% ====================================================================
%% API functions
%% ====================================================================

-export([setup/0, cleanup/1]).

-spec setup() -> #integration_test_state{}.
setup() ->
    {ok, CurrentDir} = file:get_cwd(),
    ErlangExecutablePath = os:find_executable("erl"),
    BackendArgs = prepare_args(?BACKEND_ARGS, ?BACKEND_NODE),
    BackendDir = filename:join([CurrentDir, "backend_ebin"]),
    BackendSettings = [{line, ?MAX_LINE_LENGTH}, {cd, BackendDir}, stream, use_stdio, exit_status, stderr_to_stdout],
    Backend = open_port({spawn, ErlangExecutablePath ++ BackendArgs}, BackendSettings),
    true = wait_process(?BACKEND_NODE, ?BACKEND_PROCESS, 10, 500),
    FrontendArgs = prepare_args(?FRONTEND_ARGS, ?FRONTEND_NODE),
    FrontendDir = filename:join([CurrentDir, "frontend_ebin"]),
    FrontendSettings = [{line, ?MAX_LINE_LENGTH}, {cd, FrontendDir}, stream, use_stdio, exit_status, stderr_to_stdout],
    Frontend = open_port({spawn, ErlangExecutablePath ++ FrontendArgs}, FrontendSettings),
    true = wait_process(?FRONTEND_NODE, ?FRONTEND_PROCESS, 10, 500),
    TerminalCmd = filename:join([CurrentDir, "cli_terminal_bin", "cli_terminal"]) ++ " < " ++ ?INPUT_DATA,
    #integration_test_state{backend = Backend, frontend = Frontend, terminal_cmd = TerminalCmd}.

-spec cleanup(State :: #integration_test_state{}) -> 'ok'.
cleanup(#integration_test_state{backend = Backend, frontend = Frontend}) ->
    port_close(Frontend),
    rpc:call(?FRONTEND_NODE, init, stop, []),
    wait_node_exit(?FRONTEND_NODE, 10, 500),
    port_close(Backend),
    rpc:call(?BACKEND_NODE, init, stop, []),
    wait_node_exit(?BACKEND_NODE, 10, 500),
    ok.

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec prepare_args(FormatArgsStr :: string(), Node :: atom()) -> string().
prepare_args(FormatArgsStr, Node) ->
    lists:flatten(io_lib:format(FormatArgsStr, [atom_to_list(Node)])).

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