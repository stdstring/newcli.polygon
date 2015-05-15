%% @author std-string

-module(code_generator_helper).

-export([generate_trap_guard_command/1,
         generate_finish_exec/2,
         generate_send_output_fun/1,
         generate_fsm_notification/2,
         generate_client_handler_send_error/2,
         generate_io_buffer_send_error/2,
         generate_command_fail_message/1]).

-include("code_generator_defs.hrl").
-include("code_generator_impl_defs.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

%% process_flag(trap_exit, TrapExit)
-spec generate_trap_guard_command(TrapExit :: boolean) -> tuple().
generate_trap_guard_command(TrapExit) ->
    GuardArgs = [{atom, 0, trap_exit}, {atom, 0, TrapExit}],
    {call, 0, {remote, 0, {atom, 0, erlang}, {atom, 0, process_flag}}, GuardArgs}.

%% client_handler:finish_exec(ClientHandler, ReturnValue),
-spec generate_finish_exec(ReturnValue :: tuple(), ModuleDefs :: #module_defs{}) -> tuple().
generate_finish_exec(ReturnValue, ModuleDefs) ->
    FinishExecArgs = [?CLIENT_HANDLER_VAR, ReturnValue, ?CONTEXT_VAR],
    {call, 0, {remote, 0, ?CLIENT_HANDLER_MODULE(ModuleDefs), {atom, 0, finish_exec}}, FinishExecArgs}.

%% send_output(Buffer, ClientHandler) ->
%%     ProcessFun = fun({output, Message}) -> client_handler:send_output(ClientHandler, Message) end;
%%                  fun({error, Message}) -> client_handler:send_error(ClientHandler, Message) end,
%%     Data = io_buffer:get_data(Buffer, both),
%%     lists:foreach(ProcessFun, Data).
-spec generate_send_output_fun(ModuleDefs :: #module_defs{}) -> tuple().
generate_send_output_fun(ModuleDefs) ->
    DataRetrieveArgs = [?BUFFER_VAR, {atom, 0, both}],
    DataRetrieve = {call, 0, {remote, 0, ?IO_BUFFER_MODULE(ModuleDefs), {atom, 0, get_data}}, DataRetrieveArgs},
    DataDef = {match, 0, {var, 0, 'Data'}, DataRetrieve},
    SendOutputPattern = [{tuple, 0, [{atom, 0, output}, ?MESSAGE_VAR]}],
    SendOutputCallArgs = [?CLIENT_HANDLER_VAR, ?MESSAGE_VAR],
    SendOutputCall = {call, 0, {remote, 0, ?CLIENT_HANDLER_MODULE(ModuleDefs), {atom, 0, send_output}}, SendOutputCallArgs},
    SendOutputClause = {clause, 0, SendOutputPattern, [], [SendOutputCall]},
    SendErrorPattern = [{tuple, 0, [{atom, 0, error}, ?MESSAGE_VAR]}],
    SendErrorCallArgs = [?CLIENT_HANDLER_VAR, ?MESSAGE_VAR],
    SendErrorCall = {call, 0, {remote, 0, ?CLIENT_HANDLER_MODULE(ModuleDefs), {atom, 0, send_error}}, SendErrorCallArgs},
    SendErrorClause = {clause, 0, SendErrorPattern, [], [SendErrorCall]},
    ProcessFun = {'fun', 0, {clauses, [SendOutputClause, SendErrorClause]}},
    ForeachArgs = [ProcessFun, {var, 0, 'Data'}],
    Foreach = {call, 0, {remote, 0, {atom, 0, lists}, {atom, 0, foreach}}, ForeachArgs},
    Body = [DataDef, Foreach],
    FunClause = {clause, 0, [?BUFFER_VAR, ?CLIENT_HANDLER_VAR], [], Body},
    {function, 0, ?SEND_OUTPUT_FUN, 2, [FunClause]}.

%% cli_fsm:process_command(CliFsm, CommandName),
-spec generate_fsm_notification(CommandName :: atom(), ModuleDefs :: #module_defs{}) -> tuple().
generate_fsm_notification(CommandName, ModuleDefs) ->
    NotificationArgs = [{var, 0, 'CliFsm'}, {atom, 0, CommandName}],
    {call, 0, {remote, 0, ?CLI_FSM_MODULE(ModuleDefs), {atom, 0, process_command}}, NotificationArgs}.

%% client_handler:send_error(ClientHandler, Message),
-spec generate_client_handler_send_error(ModuleDefs :: #module_defs{}, Message :: tuple()) -> tuple().
generate_client_handler_send_error(ModuleDefs, Message) ->
    generate_send_error_impl(?CLIENT_HANDLER_MODULE(ModuleDefs), ?CLIENT_HANDLER_VAR, Message).

%% io_buffer:send_error(ClientHandler, Message),
-spec generate_io_buffer_send_error(ModuleDefs :: #module_defs{}, Message :: tuple()) -> tuple().
generate_io_buffer_send_error(ModuleDefs, Message) ->
    generate_send_error_impl(?IO_BUFFER_MODULE(ModuleDefs), ?BUFFER_VAR, Message).

%% Message = string_utils:format("Command execution failed. Return code is ~w\n", [ReturnValue])
-spec generate_command_fail_message(ReturnValue :: tuple()) -> tuple().
generate_command_fail_message(ReturnValue) ->
    Args = [{string, 0, ?COMMAND_FAIL_TEMPLATE}, {cons, 0, ReturnValue, {nil, 0}}],
    {call, 0, {remote, 0, {atom, 0, string_utils}, {atom, 0, format}}, Args}.

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec generate_send_error_impl(Module :: tuple(), Ref :: tuple(), Message :: tuple()) -> tuple().
generate_send_error_impl(Module, Ref, Message) ->
    SendErrorCallArgs = [Ref, Message],
    {call, 0, {remote, 0, Module, {atom, 0, send_error}}, SendErrorCallArgs}.