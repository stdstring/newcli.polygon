-module(some_integration_tests).

-include_lib("eunit/include/eunit.hrl").

-define(MAX_LINE_LENGTH, 1000).
-define(BACKEND_NODE, 'backend_node@polygon-vm').
-define(BACKEND_PROCESS, global_input_endpoint).
-define(BACKEND_ARGS, " -noshell -sname ~s -eval \"application:start(cli_backend_application)\"").
-define(FRONTEND_NODE, 'frontend_node@polygon-vm').
-define(FRONTEND_PROCESS, cli_terminal_listen_endpoint).
-define(FRONTEND_ARGS, " -noshell -sname ~s -eval \"application:start(cli_frontend_application)\"").

-record(state, {backend = undefined :: 'undefined' | pid(), frontend = undefined :: 'undefined' | pid()}).

example_test_() ->
    {foreach, fun() -> setup() end, fun(State) -> cleanup(State) end, [{"example", fun() -> test_example() end}]}.

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
    #state{backend = Backend, frontend = Frontend}.

cleanup(#state{backend = Backend, frontend = Frontend}) ->
    port_close(Frontend),
    rpc:call(?FRONTEND_NODE, init, stop, []),
    wait_node_exit(?FRONTEND_NODE, 10, 500),
    port_close(Backend),
    rpc:call(?BACKEND_NODE, init, stop, []),
    wait_node_exit(?BACKEND_NODE, 10, 500),
    ok.

test_example() ->
    exchange_data(),
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

exchange_data() ->
    Address = {127, 0, 0, 1},
    {ok, Socket} = gen_tcp:connect(Address, 6666, [binary, {packet, 4}, {active, false}]),
    gen_tcp:send(Socket, term_to_binary({command, "?"})),
    {ok, OutputPacket} = gen_tcp:recv(Socket, 0),
    io:format(user, "Output = ~p~n", [binary_to_term(OutputPacket)]),
    {ok, EndPacket} = gen_tcp:recv(Socket, 0),
    io:format(user, "End = ~p~n", [binary_to_term(EndPacket)]),
    ok.