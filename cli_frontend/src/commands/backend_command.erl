%% @author std-string

-module(backend_command).

-behaviour(command_behaviour).

-include("common_defs.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

-export([get_name/0, get_command_body/0, execute/3]).
%% proc_lib export
-export([execute_impl/3]).

-spec get_name() -> atom().
get_name() -> backend_command.

-spec get_command_body() -> [string()].
get_command_body() -> [].

-spec execute(CommandLineRest :: string(), ClientHandler :: pid(), ExecutionState :: #execution_state{}) -> CommandPid :: pid() | {'error', Reason :: term()}.
execute(CommandLineRest, ClientHandler, ExecutionState) ->
    proc_lib:start_link(?MODULE, execute_impl, [CommandLineRest, ClientHandler, ExecutionState]).

-spec execute_impl(CommandLineRest :: string(), ClientHandler :: pid(), ExecutionState :: #execution_state{}) -> 'ok'.
execute_impl(CommandLineRest, ClientHandler, ExecutionState) ->
    proc_lib:init_ack(self()),
    {ReturnCode, NewExecutionState} = backend_command_inner:execute(CommandLineRest, ClientHandler, ExecutionState),
    client_handler:finish_command(ClientHandler, NewExecutionState, ReturnCode),
    ok.

%% ====================================================================
%% Internal functions
%% ====================================================================
