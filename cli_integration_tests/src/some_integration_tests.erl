-module(some_integration_tests).

-include_lib("eunit/include/eunit.hrl").

-define(MAX_LINE_LENGTH, 1000).
-define(BACKEND_NODE, 'backend_node@polygon-vm').
-define(BACKEND_PROCESS, global_input_endpoint).
-define(BACKEND_ARGS, " -noshell -sname ~s -s entry_point start").
-define(FRONTEND_NODE, 'frontend_node@polygon-vm').
-define(FRONTEND_PROCESS, cli_terminal_listen_endpoint).
-define(FRONTEND_ARGS, " -noshell -sname ~s -s entry_point start").

-export([prepare_args/2, wait_process/4, wait_node_exit/3]).

example_test() ->
    io:format(user, "~nexample start:~n", []),
    {ok, CurrentDir} = file:get_cwd(),
    io:format(user, "~nCurrentDir = ~p~n", [CurrentDir]),
    ErlangExecutablePath = os:find_executable("erl"),
    io:format(user, "ErlangExecutablePath = ~p~n", [ErlangExecutablePath]),
    BackendArgs = prepare_args(?BACKEND_ARGS, ?BACKEND_NODE),
    io:format(user, "BackendArgs = ~p~n", [BackendArgs]),
    BackendDir = filename:join([CurrentDir, "backend_ebin"]),
    io:format(user, "BackendDir = ~p~n", [BackendDir]),
    BackendSettings = [{line, ?MAX_LINE_LENGTH}, {cd, BackendDir}, stream, use_stdio, exit_status, stderr_to_stdout],
    Backend = open_port({spawn, ErlangExecutablePath ++ BackendArgs}, BackendSettings),
    true = wait_process(?BACKEND_NODE, ?BACKEND_PROCESS, 10, 500),
    io:format(user, "Backend = ~p~n", [Backend]),
    CommandsInfo = gen_server:call({?BACKEND_PROCESS, ?BACKEND_NODE}, {commands_info}),
    io:format(user, "CommandsInfo = ~p~n", [CommandsInfo]),
    FrontendArgs = prepare_args(?FRONTEND_ARGS, ?FRONTEND_NODE),
    io:format(user, "FrontendArgs = ~p~n", [FrontendArgs]),
    FrontendDir = filename:join([CurrentDir, "frontend_ebin"]),
    io:format(user, "FrontendDir = ~p~n", [FrontendDir]),
    FrontendSettings = [{line, ?MAX_LINE_LENGTH}, {cd, FrontendDir}, stream, use_stdio, exit_status, stderr_to_stdout],
    Frontend = open_port({spawn, ErlangExecutablePath ++ FrontendArgs}, FrontendSettings),
    true = wait_process(?FRONTEND_NODE, ?FRONTEND_PROCESS, 10, 500),
    io:format(user, "Frontend = ~p~n", [Frontend]),
    %% cleanup
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
        {badrpc,nodedown} -> false;
        unfefined -> false;
        _Pid -> true
    end;
wait_process(Node, Process, Count, WaitTime) ->
    case rpc:call(Node, erlang, whereis, [Process]) of
        {badrpc,nodedown} ->
            timer:sleep(WaitTime),
            wait_process(Node, Process, Count-1, WaitTime);
        unfefined ->
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