%% @author std-string

-module(command_sync_executer).

-include("authentication_defs.hrl").
-include("code_generator_defs.hrl").
-include("command_defs.hrl").
-include("common_defs.hrl").
-include("io_buffer_defs.hrl").

-export([execute/5]).

%% TODO (std_string) : move into appropriate place
-define(COMMAND_NOTFOUND_TEMPLATE, "Command \"~p\" does not found\n").

%% ====================================================================
%% API functions
%% ====================================================================

-spec execute(CommandName :: atom(), CommandArgs :: [term()], State :: #client_handler_state{}, IoBuffer :: pid(), IoBufferModule :: atom()) ->
    {ExecutionState :: atom(), State :: #client_handler_state{}}.
execute(CommandName, CommandArgs, State, IoBuffer, IoBufferModule) ->
    {ExecutionState, NewState} = execute_impl(CommandName, CommandArgs, State, IoBuffer),
    process_buffer_content(NewState, IoBuffer, IoBufferModule),
    {ExecutionState, NewState}.

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec execute_impl(CommandName :: atom(), CommandArgs :: [term()], State :: #client_handler_state{}, IoBuffer :: pid()) ->
    {ExecutionState :: atom(), State :: #client_handler_state{}}.
execute_impl(CommandName, CommandArgs, State, IoBuffer) ->
    GlobalConfig = State#client_handler_state.config,
    case search_command(CommandName, GlobalConfig) of
        false ->
            process_search_error(CommandName, IoBuffer),
            {?EX_CONTINUE, State};
        {true, CommandName, CommandModule} ->
            ExecContext = execution_context_factory:create(State),
            {ReturnCode, NewExecContext} = CommandModule:execute(CommandArgs, IoBuffer, IoBuffer, ExecContext),
            if
                ReturnCode == 0 ->
                    ExecutionState = get_execution_state(NewExecContext),
                    {ExecutionState, State};
                ReturnCode /= 0 ->
                    process_command_error(ReturnCode, IoBuffer),
                    ExecutionState = get_execution_state(NewExecContext),
                    {ExecutionState, State}
            end
    end.

-spec search_command(CommandName :: atom(), Config :: #global_config{}) ->
    {'true', CommandName :: atom(), CommandModule :: atom()} | 'false'.
search_command(CommandName, Config) ->
    Commands = Config#global_config.commands,
    case list_utils:get_value_by_key_with_default(Commands, CommandName, 1, false) of
        false -> false;
        {CommandName, CommandModule} -> {true, CommandName, CommandModule}
    end.

-spec process_search_error(CommandName :: atom(), IoBuffer :: pid()) -> 'ok'.
process_search_error(CommandName, IoBuffer) ->
    ErrorMessage = string_utils:format(?COMMAND_NOTFOUND_TEMPLATE, [CommandName]),
    command_utils:send_error(IoBuffer, ErrorMessage),
    ok.

-spec process_command_error(ReturnCode :: integer(), IoBuffer :: pid()) -> 'ok'.
process_command_error(ReturnCode, IoBuffer) ->
    ErrorMessage = string_utils:format(?COMMAND_FAIL_TEMPLATE, [ReturnCode]),
    command_utils:send_error(IoBuffer, ErrorMessage),
    ok.

-spec process_buffer_content(State :: #client_handler_state{}, IoBuffer :: pid(), IoBufferModule :: atom()) -> 'ok'.
process_buffer_content(State, IoBuffer, IoBufferModule) ->
    Endpoint = State#client_handler_state.endpoint,
    Messages = IoBufferModule:get_data(both),
    ProcessFun = fun(#output{message = Message}) -> cli_terminal_endpoint:handle_output(Endpoint, Message);
                    (#error{message = Message}) -> cli_terminal_endpoint:handle_error(Endpoint, Message) end,
    lists:foreach(ProcessFun, Messages),
    IoBufferModule:reset(IoBuffer),
    ok.

-spec get_execution_state(ExecContext :: [{Key :: atom(), Value :: term()}]) -> atom().
get_execution_state(ExecContext) ->
    list_utils:get_value_by_key_with_default(ExecContext, ?EX_STATE_KEY, 1, ?EX_CONTINUE).