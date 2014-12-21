%% @author std-string

-module(command_mock).

-export([get_name/0, execute/4]).

%% ====================================================================
%% API functions
%% ====================================================================

get_name() ->
    command_example.

execute(CommandArgs, Stdout, Stderr, Context) ->
    mock_server:execute(command_module, execute, [CommandArgs, Stdout, Stderr, Context]).

%% ====================================================================
%% Internal functions
%% ====================================================================