%% @author std-string

-module(client_handler_mock).

-export([start/2, process_command/2, interrupt_command/1, get_current_state/1, get_extensions/2, exit/1, send_output/2, send_error/2, finish_command/2]).

%% ====================================================================
%% API functions
%% ====================================================================

start(GlobalConfig, Endpoint) ->
    mock_server:execute(client_handler, start, [GlobalConfig, Endpoint]).

process_command(Handler, CommandLine) ->
    mock_server:execute(client_handler, process_command, [Handler, CommandLine]).

interrupt_command(Handler) ->
    mock_server:execute(client_handler, interrupt_command, [Handler]).

get_current_state(Handler) ->
    mock_server:execute(client_handler, get_current_state, [Handler]).

get_extensions(Handler, CommandLine) ->
    mock_server:execute(client_handler, get_extensions, [Handler, CommandLine]).

send_output(Handler, Output) ->
    mock_server:execute(client_handler, send_output, [Handler, Output]).

send_error(Handler, Error) ->
    mock_server:execute(client_handler, send_error, [Handler, Error]).

finish_command(Handler, ReturnCode) ->
    mock_server:execute(client_handler, finish_command, [Handler, ReturnCode]).

exit(Handler) ->
    mock_server:execute(client_handler, exit, [Handler]).

%% ====================================================================
%% Internal functions
%% ====================================================================