%% @author std-string

-module(backend_command).

-behaviour(command_behaviour).
-behaviour(gen_server).

-include("message_defs.hrl").
-include("common_defs.hrl").

-record(command_state, {commandline_rest = ""}).
-record(command_result, {completion_code = 0, execution_state = undefined}).

%% -record(start_command, {execution_state = undefined}).

%% ====================================================================
%% API functions
%% ====================================================================

-export([get_name/0, get_command_body/0, create/1, execute/2]).
%% gen_server export
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-spec get_name() -> atom().
get_name() -> backend_command.

-spec get_command_body() -> [string()].
get_command_body() -> [].

-spec create(CommandLineRest :: string()) -> {'ok', Command :: pid()} | {'error', Reason :: term()}.
create(CommandLineRest) ->
    {error, not_implemented}.

-spec execute(Command :: pid(), ExecutionState :: #execution_state{}) -> {ReturnCode :: integer(), ExecutionState :: #execution_state{}}.
execute(Command, ExecutionState) ->
    {0, ExecutionState}.

init(CommandLineRest) ->
    State = #command_state{commandline_rest = CommandLineRest},
    {ok, State}.

handle_call({execute, ExecutionState}, _From, State) ->
    Message = State#command_state.commandline_rest,
    Session = ExecutionState#execution_state.session,
    %% gen_server:call(Session, #command{message = Message}),
    error(not_implemented).

handle_cast(_Request, State) -> {stop, enotsup, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> true.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================

execute_impl(ExecutionState) ->
    receive
        #command_output{message = Message} ->
            io:format("~s", [Message]),
            execute_impl(ExecutionState);
        #command_error{message = Message} ->
            io:format(standard_error, "~s", [Message]),
            execute_impl(ExecutionState);
        #command_end{completion_code = CompletionCode, cli_mode = CliMode} ->
            NewExecutionState = ExecutionState#execution_state{current_cli_mode = CliMode},
            {CompletionCode, NewExecutionState};
        #command_fail{reason = Reason, cli_mode = CliMode} ->
            io:format(standard_error, "Command's execution is failed due to the following: ~p~n", [Reason]),
            NewExecutionState = ExecutionState#execution_state{current_cli_mode = CliMode},
            {255, NewExecutionState}
    end.

