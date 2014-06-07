%% @author std-string

-module(integration_tests_manager).

-include("integration_tests_defs.hrl").

-define(MAX_LINE_LENGTH, 1000).
-define(BACKEND_NODE, 'backend_node@polygon-vm').
-define(FRONTEND_NODE, 'frontend_node@polygon-vm').

%% ====================================================================
%% API functions
%% ====================================================================

-export([setup/0, cleanup/1]).

setup() ->
    {ok, CurrentDir} = file:get_cwd(),
    ErlangExecutablePath = os:find_executable("erl"),
    BackendArgs = prepare_args(" -noshell -sname ~s -s entry_point start", ?BACKEND_NODE),
    BackendDir = filename:join([CurrentDir, "backend_ebin"]),    
    BackendSettings = [{line, ?MAX_LINE_LENGTH}, {cd, BackendDir}, stream, use_stdio, exit_status, stderr_to_stdout],
    Backend = open_port({spawn, ErlangExecutablePath ++ BackendArgs}, BackendSettings),
    true = wait_process(?BACKEND_NODE, global_input_endpoint, 10, 1000),
    cli_backend_life_manager:start(?BACKEND_NODE),
    FrontendArgs = prepare_args(" -noshell -sname ~s -pa ./frontend_ebin -run cli_frontend_application main ./frontend_data/frontend.conf -s init stop < " ++ ?INPUT_DATA, ?FRONTEND_NODE),
    FrontendCmd = ErlangExecutablePath ++ FrontendArgs,
    #integration_test_state{backend = Backend, frontend_cmd = FrontendCmd}.

cleanup(#integration_test_state{backend = Backend}) ->
    port_close(Backend),
    cli_backend_life_manager:stop(?BACKEND_NODE).

prepare_args(FormatArgsStr, Node) ->
    lists:flatten(io_lib:format(FormatArgsStr, [atom_to_list(Node)])).

wait_process(Node, ProcessName, 0, _WaitTime) ->
    case rpc:call(Node, erlang, whereis, [ProcessName]) of
        {badrpc,nodedown} -> false;
        unfefined -> false;
        _Pid -> true
    end;
wait_process(Node, ProcessName, Count, WaitTime) ->
    case rpc:call(Node, erlang, whereis, [ProcessName]) of
        {badrpc,nodedown} ->
            timer:sleep(WaitTime),
            wait_process(Node, ProcessName, Count-1, WaitTime);
        unfefined ->
            timer:sleep(WaitTime),
            wait_process(Node, ProcessName, Count-1, WaitTime);
        _Pid -> true
    end.