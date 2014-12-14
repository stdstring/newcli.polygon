%% @author std-string

-module(cli_fsm_mock).

-export([start/1, process_command/2, get_current_state/1]).

%% ====================================================================
%% API functions
%% ====================================================================

start(SourceData) ->
    mock_server:execute(cli_fsm, start, [SourceData]).

process_command(FsmPid, CommandName) ->
    mock_server:execute(cli_fsm, process_command, [FsmPid, CommandName]).

get_current_state(FsmPid) ->
    mock_server:execute(cli_fsm, get_current_state, [FsmPid]).

%% ====================================================================
%% Internal functions
%% ====================================================================