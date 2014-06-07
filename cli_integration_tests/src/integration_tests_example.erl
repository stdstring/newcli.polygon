-module(integration_tests_example).

-include_lib("eunit/include/eunit.hrl").

-define(MAX_LINE_LENGTH, 1000).
-define(BACKEND_NODE, 'backend_node@polygon-vm').
-define(FRONTEND_NODE, 'frontend_node@polygon-vm').

integration_test() ->
    ?debugHere,
    {ok, CurrentDir} = file:get_cwd(),
    ErlangExecutablePath = os:find_executable("erl"),
    BackendArgs = prepare_args(" -noshell -sname ~s -s entry_point start", ?BACKEND_NODE),
    BackendDir = filename:join([CurrentDir, "backend_ebin"]),    
    BackendSettings = [{line, ?MAX_LINE_LENGTH}, {cd, BackendDir}, stream, use_stdio, exit_status, stderr_to_stdout],
    Backend = open_port({spawn, ErlangExecutablePath ++ BackendArgs}, BackendSettings),
    true = wait_process(?BACKEND_NODE, global_input_endpoint, 10, 1000),
    cli_backend_life_manager:start(?BACKEND_NODE),
    ?debugHere,
    InputData = "i?\ninterface ?\n",
    ?assertEqual(ok, file:write_file("/tmp/input", InputData)),
    FrontendDir = filename:join([CurrentDir, "frontend_ebin"]),
    FrontendDataDir = filename:join([CurrentDir, "frontend_data"]),
    FrontendArgs = prepare_args(" -noshell -sname ~s -pa " ++ FrontendDir ++ " -run cli_frontend_application main " ++ FrontendDataDir ++ "/frontend.conf -s init stop < /tmp/input", ?FRONTEND_NODE),
    ?debugMsg(FrontendArgs),
    OutputData = os:cmd(ErlangExecutablePath ++ FrontendArgs),
    ?debugFmt("OutputData: ~p~n", [OutputData]),
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