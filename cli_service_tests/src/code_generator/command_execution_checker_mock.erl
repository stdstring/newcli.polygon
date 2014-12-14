%% @author std-string

-module(command_execution_checker_mock).

-export([execution_precheck/3]).

%% ====================================================================
%% API functions
%% ====================================================================

execution_precheck(CommandName, CliFsm, User) ->
    mock_server:execute(command_execution_checker, execution_precheck, [CommandName, CliFsm, User]).

%% ====================================================================
%% Internal functions
%% ====================================================================