%% @author std-string

-module(integration_tests_manager).

-include_lib("eunit/include/eunit.hrl").

-include("integration_tests_defs.hrl").

-define(MAX_LINE_LENGTH, 1000).
-define(BACKEND_NODE, 'backend_node@polygon-vm').
-define(BACKEND_PROCESS, global_input_endpoint).
-define(FRONTEND_NODE, 'frontend_node@polygon-vm').

%% ====================================================================
%% API functions
%% ====================================================================

-export([setup/0, cleanup/1]).

-spec setup() -> #integration_test_state{}.
setup() ->
    {ok, CurrentDir} = file:get_cwd(),
    ErlangExecutablePath = os:find_executable("erl"),
    BackendArgs = prepare_args(" -noshell -sname ~s -s entry_point start", ?BACKEND_NODE),
    BackendDir = filename:join([CurrentDir, "backend_ebin"]),    
    BackendSettings = [{line, ?MAX_LINE_LENGTH}, {cd, BackendDir}, stream, use_stdio, exit_status, stderr_to_stdout],
    Backend = open_port({spawn, ErlangExecutablePath ++ BackendArgs}, BackendSettings),
    true = wait_process(?BACKEND_PROCESS, 10, 500),
    FrontendArgs = prepare_args(" -noshell -sname ~s -pa ./frontend_ebin -run cli_frontend_application main ./frontend_data/frontend.conf -s init stop < " ++ ?INPUT_DATA, ?FRONTEND_NODE),
    FrontendCmd = ErlangExecutablePath ++ FrontendArgs,
    #integration_test_state{backend = Backend, frontend_cmd = FrontendCmd}.

-spec cleanup(State :: #integration_test_state{}) -> 'ok'.
cleanup(#integration_test_state{backend = Backend}) ->
    port_close(Backend),
    rpc:call(?BACKEND_NODE, init, stop, []),
    wait_node_exit(10, 500),
    ok.

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec prepare_args(FormatArgsStr :: string(), Node :: atom()) -> string().
prepare_args(FormatArgsStr, Node) ->
    lists:flatten(io_lib:format(FormatArgsStr, [atom_to_list(Node)])).

-spec wait_process(ProcessName :: atom(), Count :: integer(), WaitTime :: integer()) -> boolean().
wait_process(ProcessName, 0, _WaitTime) ->
    case rpc:call(?BACKEND_NODE, erlang, whereis, [ProcessName]) of
        {badrpc,nodedown} -> false;
        unfefined -> false;
        _Pid -> true
    end;
wait_process(ProcessName, Count, WaitTime) ->
    case rpc:call(?BACKEND_NODE, erlang, whereis, [ProcessName]) of
        {badrpc,nodedown} ->
            timer:sleep(WaitTime),
            wait_process(ProcessName, Count-1, WaitTime);
        unfefined ->
            timer:sleep(WaitTime),
            wait_process(ProcessName, Count-1, WaitTime);
        _Pid -> true
    end.

-spec wait_node_exit(Count :: integer(), WaitTime :: integer()) -> boolean().
wait_node_exit(0, _WaitTime) ->
    case net_adm:ping(?BACKEND_NODE) of
        pang -> true;
        pong -> false
    end;
wait_node_exit(Count, WaitTime) ->
    case net_adm:ping(?BACKEND_NODE) of
        pong ->
            timer:sleep(WaitTime),
            wait_node_exit(Count-1, WaitTime);
        pang -> true
    end.