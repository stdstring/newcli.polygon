-module(integration_example).

-export([start/0]).

-define(MAX_LINE_LENGTH, 1000).

start() ->
    {ok, CurrentDir} = file:get_cwd(),
    ErlangExecutablePath = os:find_executable("erl"),
    BackendArgs = " -noshell -sname backend_node -s entry_point start",
    BackendDir = filename:join([CurrentDir, "backend_ebin"]),    
    %%BackendSettings = [{line, ?MAX_LINE_LENGTH}, {cd, BackendDir}, stream, use_stdio, exit_status, stderr_to_stdout],
    BackendSettings = [{line, ?MAX_LINE_LENGTH}, {cd, BackendDir}, stream, use_stdio],
    Backend = open_port({spawn, ErlangExecutablePath ++ BackendArgs}, BackendSettings),
    timer:sleep(20000),
    BackendPingResult = net_adm:ping('backend_node@polygon-vm'),
    io:format("BackendPingResult: ~p~n", [BackendPingResult]),
    CommandsInfo = gen_server:call({global_input_endpoint, 'backend_node@polygon-vm'}, {commands_info}),
    io:format("CommandsInfo: ~p~n", [CommandsInfo]),
    FrontendArgs = " -noshell -sname frontend_node -run cli_frontend_application main ../frontend_data/frontend.conf -s init stop",
    FrontendDir = filename:join([CurrentDir, "frontend_ebin"]),
    FrontendSettings = [{line, ?MAX_LINE_LENGTH}, {cd, FrontendDir}, stream, use_stdio],
    %%FrontendSettings = [{line, ?MAX_LINE_LENGTH}, {cd, FrontendDir}, stream, use_stdio, exit_status, stderr_to_stdout],
    Frontend = open_port({spawn, ErlangExecutablePath ++ FrontendArgs}, FrontendSettings),
    timer:sleep(20000),
    FrontendPingResult = net_adm:ping('frontend_node@polygon-vm'),
    io:format("FrontendPingResult: ~p~n", [FrontendPingResult]),
    port_command(Frontend, "i?\n"),
    port_command(Frontend, "interface ?\n"),
    true = wait_message(Frontend, "@CliDemo>CommandLine: \"i?\\n\"", 20000),
    true = wait_message(Frontend, "interface\tinterface range", 20000),
    true = wait_message(Frontend, "@CliDemo>CommandLine: \"interface ?\\n\"", 20000),
    true = wait_message(Frontend, "interface {interface-id} command", 20000),
    %%{#Port<0.680>,{data,{eol,"@CliDemo>CommandLine: \"i?\\n\""}}},
    %%{#Port<0.680>,{data,{eol,"interface\tinterface range"}}},
    %%{#Port<0.680>,{data,{eol,"@CliDemo>CommandLine: \"interface ?\\n\""}}},
    %%{#Port<0.680>,{data,{eol,"interface {interface-id} command"}}}
    port_close(Backend),
    port_close(Frontend).

wait_message(SourcePort, MessageBody, Timeout) ->
    receive
        {SourcePort, {data, {eol, MessageBody}}} -> true
    after Timeout -> false
    end.