%% @author std-string

-module(command_mock).

-export([get_name/0, execute/1]).

%% ====================================================================
%% API functions
%% ====================================================================

get_name() ->
    command_example.

execute(Args) ->
    mock_server:execute(command_module, execute, [Args]).

%% ====================================================================
%% Internal functions
%% ====================================================================