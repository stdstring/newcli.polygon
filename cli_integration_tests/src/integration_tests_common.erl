%% @author std-string

-module(integration_tests_common).

-include_lib("eunit/include/eunit.hrl").

-export([start_cli_service/0, stop_cli_service/1]).

-include("integration_tests_defs.hrl").

-define(MAX_LINE_LENGTH, 1000).

%% ====================================================================
%% API functions
%% ====================================================================

-spec start_cli_service() -> port().
start_cli_service() ->
    {ok, CurrentDir} = file:get_cwd(),
    ErlangExecutablePath = os:find_executable("erl"),
    ServiceArgs = string_utils:format(?SERVICE_ARGS, [?SERVICE_NODE]),
    ServiceDir = filename:join([CurrentDir, "service_ebin"]),
    ServiceSettings = [{line, ?MAX_LINE_LENGTH}, {cd, ServiceDir}, stream, use_stdio, exit_status, stderr_to_stdout],
    Service = open_port({spawn, ErlangExecutablePath ++ ServiceArgs}, ServiceSettings),
    io:format(user, "Service = ~p~n", [Service]),
    true = wait_process(?SERVICE_NODE, ?SERVICE_PROCESS, 10, 500),
    Service.

-spec stop_cli_service(Service :: port()) -> 'ok'.
stop_cli_service(Service) ->
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