%% @author std-string

-module(code_generator).

-export([generate/4]).

-include("code_generator_defs.hrl").
-include("command_defs.hrl").
-include("frame_defs.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

%% expectation in pseudocode :
%%
%% CommandName = ...
%% CommandModule = ...
%% CommandArgs = ...
%% entry_func(CliFsm, ClientHandler, Context) ->
%%     case io_buffer:start() of
%%         {error, Reason} -> process_buffer_fail(ClientHandler, Context);
%%         {ok, Buffer} ->
%%             User = list_utils:get_value_by_key_with_default(Context, user, 1, undefined),
%%             case command_execution_checker:execution_precheck(CommandNam, CliFsm, User) of
%%                 {false, access_denied} -> process_fail("access denied message", 255, Buffer, ClientHandler, Context);
%%                 {false, authorization_bad_config} -> process_fail("bad config message", 255, Buffer, ClientHandler, Context);
%%                 {false, unsuitable_command} -> process_fail("unsuitable command message", 255, Buffer, ClientHandler, Context);
%%                 true -> process_command(CliFsm, Buffer, ClientHandler, Context)
%%             end
%%     end.
%%
%% process_buffer_fail(ClientHandler, Context) ->
%%     process_flag(trap_exit, true),
%%     client_handler:send_error(ClientHandler, "buffer initialization message"),
%%     Message = string_utils:format("Command execution failed. Return code is ~w\n", [255]),
%%     client_handler:send_error(ClientHandler, Message),
%%     client_handler:finish_exec(ClientHandler, 255)
%%     process_flag(trap_exit, false),
%%     {255, Context}.
%%
%% process_command(CliFsm, Buffer, ClientHandler, Context) ->
%%     case apply(CommandModule, execute, [CommandArgs, Buffer, Buffer, Context]) of
%%         {0, NewContext} ->
%%             process_success(CliFsm, Buffer, ClientHandler, NewContext);
%%         {ReturnValue, NewContext} ->
%%             process_fail(ReturnValue, Buffer, ClientHandler, NewContext)
%%     end.
%%
%% process_success(CliFsm, Buffer, ClientHandler, Context) ->
%%     process_flag(trap_exit, true),
%%     cli_fsm:process_command(CliFsm, CommandName),
%%     send_output(Buffer, ClientHandler),
%%     client_handler:finish_exec(ClientHandler, 0),
%%     process_flag(trap_exit, false),
%%     {0, Context}.
%%
%% process_fail(ReturnValue, Buffer, ClientHandler, Context) ->
%%     process_flag(trap_exit, true),
%%     Message = string_utils:format("Command execution failed. Return code is ~w\n", [ReturnValue]),
%%     io_buffer:send_error(Buffer, Message),
%%     send_output(Buffer, ClientHandler),
%%     client_handler:finish_exec(ClientHandler, ReturnValue),
%%     process_flag(trap_exit, false),
%%     {ReturnValue, Context}.
%%
%% process_fail(FailMessage, ReturnValue, Buffer, ClientHandler, Context) ->
%%     process_flag(trap_exit, true),
%%     io_buffer:send_error(Buffer, FailMessage),
%%     Message = string_utils:format("Command execution failed. Return code is ~w\n", [ReturnValue]),
%%     io_buffer:send_error(Buffer, Message),
%%     send_output(Buffer, ClientHandler),
%%     client_handler:finish_exec(ClientHandler, ReturnValue),
%%     process_flag(trap_exit, false),
%%     {ReturnValue, Context}.
%%
%% send_output(Buffer, ClientHandler) ->
%%     ProcessFun = fun({output, Message}) -> client_handler:send_output(ClientHandler, Message) end;
%%                  fun({error, Message}) -> client_handler:send_error(ClientHandler, Message) end,
%%     Data = io_buffer:get_data(Buffer, both),
%%     lists:foreach(ProcessFun, Data).


-spec generate(EntryModuleName :: atom(), EntryFunName :: atom(), Command :: #command{}, ModuleDefs :: #module_defs{}) ->
    {'true', Binary :: binary()} | {'false', Reason :: term()}.
generate(EntryModuleName, EntryFunName, Command, ModuleDefs) ->
    CommandName = Command#command.name,
    ModuleForm = {attribute, 0, module, EntryModuleName},
    ExportForm = {attribute, 0, export, [{EntryFunName, 3}]},
    EntryFun = generate_entry_func(CommandName, ModuleDefs, EntryFunName),
    ProcessFun = generate_process_command_fun(Command),
    ProcessSuccessFun = generate_process_success_fun(CommandName, ModuleDefs),
    BufferFailFun = generate_process_buffer_fail_fun(ModuleDefs),
    ProcessFailFun = generate_process_fail_fun(ModuleDefs),
    ProcessCommandFailFun = generate_process_command_fail_fun(ModuleDefs),
    SendOutputFun = code_generator_helper:generate_send_output_fun(ModuleDefs),
    CompileResult = compile:forms([ModuleForm,
                                   ExportForm,
                                   EntryFun,
                                   ProcessFun,
                                   ProcessSuccessFun,
                                   BufferFailFun,
                                   ProcessFailFun,
                                   ProcessCommandFailFun,
                                   SendOutputFun]),
    case CompileResult of
        {ok, EntryModuleName, Binary} -> {true, Binary};
        _Other -> {false, undefined}
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

%% entry_func(CliFsm, ClientHandler, Context) ->
%%     case io_buffer:start() of
%%         {error, Reason} -> process_buffer_fail(ClientHandler, Context);
%%         {ok, Buffer} ->
%%             User = list_utils:get_value_by_key_with_default(Context, user, 1, undefined),
%%             case command_execution_checker:execution_precheck(CommandNam, CliFsm, dict:fetch(user, Context)) of
%%                 {false, access_denied} -> process_fail("access denied message", 255, Buffer, ClientHandler, Context);
%%                 {false, authorization_bad_config} -> process_fail("bad config message", 255, Buffer, ClientHandler, Context);
%%                 {false, unsuitable_command} -> process_fail("unsuitable command message", 255, Buffer, ClientHandler, Context);
%%                 true -> process_command(CliFsm, Buffer, ClientHandler, Context)
%%             end
%%     end.
-spec generate_entry_func(CommandName :: atom(), ModuleDefs :: #module_defs{}, EntryFunName :: atom()) -> tuple().
generate_entry_func(CommandName, ModuleDefs, EntryFunName) ->
    BufferStartErrorPattern = [{tuple, 0, [{atom, 0, error}, {var, 0, '_Reason'}]}],
    BufferStartErrorBody = [{call, 0, {atom, 0, ?PROCESS_BUFFER_FAIL_FUN}, [?CLIENT_HANDLER_VAR, ?CONTEXT_VAR]}],
    BufferStartErrorClause = {clause, 0, BufferStartErrorPattern, [], BufferStartErrorBody},
    BufferStartSuccessPattern = [{tuple, 0, [{atom, 0, ok}, ?BUFFER_VAR]}],
    BufferStartSuccessBody = [generate_precheck_command(CommandName, ModuleDefs)],
    BufferStartSuccessClause = {clause, 0, BufferStartSuccessPattern, [], BufferStartSuccessBody},
    CaseExpr = {call, 0, {remote, 0, ?IO_BUFFER_MODULE(ModuleDefs), {atom, 0, start}}, []},
    Body = [{'case', 0, CaseExpr, [BufferStartErrorClause, BufferStartSuccessClause]}],
    FunClause = {clause, 0, [?CLI_FSM_VAR, ?CLIENT_HANDLER_VAR, ?CONTEXT_VAR], [], Body},
    {function, 0, EntryFunName, 3, [FunClause]}.

%% User = list_utils:get_value_by_key_with_default(Context, user, 1, undefined),
%% case command_execution_checker:execution_precheck(CommandName, CliFsm, dict:fetch(user, Context)) of
%%     {false, access_denied} -> process_fail("access denied message", 255, Buffer, ClientHandler, Context);
%%     {false, authorization_bad_config} -> process_fail("bad config message", 255, Buffer, ClientHandler, Context);
%%     {false, unsuitable_command} -> process_fail("unsuitable command message", 255, Buffer, ClientHandler, Context);
%%     true -> process_command(CliFsm, Buffer, ClientHandler, Context)
%% end
-spec generate_precheck_command(CommandName :: atom(), ModuleDefs :: #module_defs{}) -> tuple().
generate_precheck_command(CommandName, ModuleDefs) ->
    AccessDeniedPattern = [{tuple, 0, [{atom, 0, false}, {atom, 0, access_denied}]}],
    AccessDeniedProcessArgs = [{string, 0, ?ACCESS_DENIED_MESSAGE}, {integer, 0, 255}, ?BUFFER_VAR, ?CLIENT_HANDLER_VAR, ?CONTEXT_VAR],
    AccessDeniedBody = [{call, 0, {atom, 0, ?PROCESS_FAIL_FUN}, AccessDeniedProcessArgs}],
    AccessDeniedClause = {clause, 0, AccessDeniedPattern, [], AccessDeniedBody},
    BadConfigPattern = [{tuple, 0, [{atom, 0, false}, {atom, 0, authorization_bad_config}]}],
    BadConfigProcessArgs = [{string, 0, ?BAD_CONFIG_MESSAGE}, {integer, 0, 255}, ?BUFFER_VAR, ?CLIENT_HANDLER_VAR, ?CONTEXT_VAR],
    BadConfigBody = [{call, 0, {atom, 0, ?PROCESS_FAIL_FUN}, BadConfigProcessArgs}],
    BadConfigClause = {clause, 0, BadConfigPattern, [], BadConfigBody},
    UnsuitableCommandPattern = [{tuple, 0, [{atom, 0, false}, {atom, 0, unsuitable_command}]}],
    UnsuitableCommandProcessArgs = [{string, 0, ?UNSUITABLE_COMMAND_MESSAGE}, {integer, 0, 255}, ?BUFFER_VAR, ?CLIENT_HANDLER_VAR, ?CONTEXT_VAR],
    UnsuitableCommandBody = [{call, 0, {atom, 0, ?PROCESS_FAIL_FUN}, UnsuitableCommandProcessArgs}],
    UnsuitableCommandClause = {clause, 0, UnsuitableCommandPattern, [], UnsuitableCommandBody},
    SuccessBody = [{call, 0, {atom, 0, ?PROCESS_COMMAND_FUN}, [?CLI_FSM_VAR, ?BUFFER_VAR, ?CLIENT_HANDLER_VAR, ?CONTEXT_VAR]}],
    SuccessClause = {clause, 0, [{atom, 0, true}], [], SuccessBody},
    User = generate_find_user_command(),
    CaseExprArgs = [{atom, 0, CommandName}, ?CLI_FSM_VAR, User],
    CaseExpr = {call, 0, {remote, 0, ?EXEC_CHECKER_MODULE(ModuleDefs), {atom, 0, execution_precheck}}, CaseExprArgs},
    {'case', 0, CaseExpr, [AccessDeniedClause, BadConfigClause, UnsuitableCommandClause, SuccessClause]}.

%% User = list_utils:get_value_by_key_with_default(Context, user, 1, undefined),
-spec generate_find_user_command() -> tuple().
generate_find_user_command() ->
    Args = [?CONTEXT_VAR, {atom, 0, user}, {integer, 0, 1}, {atom, 0, undefined}],
    {call, 0, {remote, 0, {atom, 0, list_utils}, {atom, 0, get_value_by_key_with_default}}, Args}.

%% process_command(CliFsm, Buffer, ClientHandler, Context) ->
%%     case apply(CommandModule, execute, [CommandArgs, Buffer, Buffer, Context]) of
%%         {0, NewContext} ->
%%             process_success(CliFsm, Buffer, ClientHandler, NewContext);
%%         {ReturnValue, NewContext} ->
%%             process_fail(ReturnValue, Buffer, ClientHandler, NewContext)
%%     end.
-spec generate_process_command_fun(Command :: #command{}) -> tuple().
generate_process_command_fun(#command{module = Module, arguments =Args}) ->
    CommandArgs = [generate_arg_list(Args), ?BUFFER_VAR, ?BUFFER_VAR, ?CONTEXT_VAR],
    CaseExpr = {call, 0, {remote, 0, {atom, 0, Module}, {atom, 0, ?COMMAND_EXEC_FUN}}, CommandArgs},
    SuccessBody = [{call, 0, {atom, 0, ?PROCESS_SUCCESS_FUN}, [?CLI_FSM_VAR, ?BUFFER_VAR, ?CLIENT_HANDLER_VAR, {var, 0, 'NewContext'}]}],
    SuccessPattern = [{tuple, 0, [{atom, 0, 0}, {var, 0, 'NewContext'}]}],
    SuccessClause = {clause, 0, SuccessPattern, [], SuccessBody},
    ProcessFailArgs = [?RETURN_VALUE_VAR, ?BUFFER_VAR, ?CLIENT_HANDLER_VAR, {var, 0, 'NewContext'}],
    FailBody = [{call, 0, {atom, 0, ?PROCESS_FAIL_FUN}, ProcessFailArgs}],
    FailPattern = [{tuple, 0, [?RETURN_VALUE_VAR, {var, 0, 'NewContext'}]}],
    FailClause = {clause, 0, FailPattern, [], FailBody},
    Body = [{'case', 0, CaseExpr, [SuccessClause, FailClause]}],
    FunClause = {clause, 0, [?CLI_FSM_VAR, ?BUFFER_VAR, ?CLIENT_HANDLER_VAR, ?CONTEXT_VAR], [], Body},
    {function, 0, ?PROCESS_COMMAND_FUN, 4, [FunClause]}.

-spec generate_arg_list(Args :: [#frame_item{}]) -> tuple().
generate_arg_list(Args) ->
    generate_arg_list(lists:reverse(Args), {nil, 0}).

-spec generate_arg_list(Args :: [#argument{}], Result :: tuple()) -> tuple().
generate_arg_list([], Result) -> Result;
generate_arg_list([#argument{type = word, value = Value} | Rest], Result) ->
    NewResult = {cons, 0, {string, 0, Value}, Result},
    generate_arg_list(Rest, NewResult);
generate_arg_list([#argument{type = string, value = Value} | Rest], Result) ->
    NewResult = {cons, 0, {string, 0, Value}, Result},
    generate_arg_list(Rest, NewResult).

%% process_success(CliFsm, Buffer, ClientHandler, Context) ->
%%     process_flag(trap_exit, true),
%%     cli_fsm:process_command(CliFsm, CommandName),
%%     send_output(Buffer, ClientHandler),
%%     client_handler:finish_exec(ClientHandler, 0),
%%     process_flag(trap_exit, false),
%%     {0, Context}.
-spec generate_process_success_fun(CommandName :: atom(), ModuleDefs :: #module_defs{}) -> tuple().
generate_process_success_fun(CommandName, ModuleDefs) ->
    SetupGuard = code_generator_helper:generate_trap_guard_command(true),
    FsmNotification = code_generator_helper:generate_fsm_notification(CommandName, ModuleDefs),
    SendOutput = {call, 0, {atom, 0, ?SEND_OUTPUT_FUN}, [?BUFFER_VAR, ?CLIENT_HANDLER_VAR]},
    FinishExec = code_generator_helper:generate_finish_exec({integer, 0, 0}, ModuleDefs),
    ReleaseGuard = code_generator_helper:generate_trap_guard_command(false),
    ReturnValue = {tuple, 0, [{integer, 0, 0}, ?CONTEXT_VAR]},
    Body = [SetupGuard, FsmNotification, SendOutput, FinishExec, ReleaseGuard, ReturnValue],
    FunClause = {clause, 0, [?CLI_FSM_VAR, ?BUFFER_VAR, ?CLIENT_HANDLER_VAR, ?CONTEXT_VAR], [], Body},
    {function, 0, ?PROCESS_SUCCESS_FUN, 4, [FunClause]}.

%% process_buffer_fail(ClientHandler, Context) ->
%%     process_flag(trap_exit, true),
%%     client_handler:send_error(ClientHandler, "buffer initialization message"),
%%     Message = string_utils:format("Command execution failed. Return code is ~w\n", [255]),
%%     client_handler:send_error(ClientHandler, Message),
%%     client_handler:finish_exec(ClientHandler, 255)
%%     process_flag(trap_exit, false),
%%     {255, Context}.
-spec generate_process_buffer_fail_fun(ModuleDefs :: #module_defs{}) -> tuple().
generate_process_buffer_fail_fun(ModuleDefs) ->
    SetupGuard = code_generator_helper:generate_trap_guard_command(true),
    SendErrorCall = code_generator_helper:generate_client_handler_send_error(ModuleDefs, {string, 0, ?BUFFER_START_FAIL_MESSAGE}),
    SendCommandFailMessage = code_generator_helper:generate_command_fail_message({integer, 0, 255}),
    SendCommandFail = code_generator_helper:generate_client_handler_send_error(ModuleDefs, SendCommandFailMessage),
    FinishExec = code_generator_helper:generate_finish_exec({integer, 0, 255}, ModuleDefs),
    ReleaseGuard = code_generator_helper:generate_trap_guard_command(false),
    ReturnValue = {tuple, 0, [{integer, 0, 255}, ?CONTEXT_VAR]},
    Body = [SetupGuard, SendErrorCall, SendCommandFail, FinishExec, ReleaseGuard, ReturnValue],
    FunClause = {clause, 0, [?CLIENT_HANDLER_VAR, ?CONTEXT_VAR], [], Body},
    {function, 0, ?PROCESS_BUFFER_FAIL_FUN, 2, [FunClause]}.

%% process_fail(ReturnValue, Buffer, ClientHandler, Context) ->
%%     process_flag(trap_exit, true),
%%     Message = string_utils:format("Command execution failed. Return code is ~w\n", [ReturnValue]),
%%     io_buffer:send_error(Buffer, Message),
%%     send_output(Buffer, ClientHandler),
%%     client_handler:finish_exec(ClientHandler, ReturnValue),
%%     process_flag(trap_exit, false),
%%     {ReturnValue, Context}.
-spec generate_process_command_fail_fun(ModuleDefs :: #module_defs{}) -> tuple().
generate_process_command_fail_fun(ModuleDefs) ->
    SetupGuard = code_generator_helper:generate_trap_guard_command(true),
    Message = code_generator_helper:generate_command_fail_message(?RETURN_VALUE_VAR),
    SendFailMessage = code_generator_helper:generate_io_buffer_send_error(ModuleDefs, Message),
    SendOutput = {call, 0, {atom, 0, ?SEND_OUTPUT_FUN}, [?BUFFER_VAR, ?CLIENT_HANDLER_VAR]},
    FinishExec = code_generator_helper:generate_finish_exec(?RETURN_VALUE_VAR, ModuleDefs),
    ReleaseGuard = code_generator_helper:generate_trap_guard_command(false),
    ReturnValue = {tuple, 0, [?RETURN_VALUE_VAR, ?CONTEXT_VAR]},
    Body = [SetupGuard, SendFailMessage, SendOutput, FinishExec, ReleaseGuard, ReturnValue],
    FunClause = {clause, 0, [?RETURN_VALUE_VAR, ?BUFFER_VAR, ?CLIENT_HANDLER_VAR, ?CONTEXT_VAR], [], Body},
    {function, 0, ?PROCESS_FAIL_FUN, 4, [FunClause]}.

%% process_fail(FailMessage, ReturnValue, Buffer, ClientHandler, Context) ->
%%     process_flag(trap_exit, true),
%%     io_buffer:send_error(Buffer, FailMessage),
%%     Message = string_utils:format("Command execution failed. Return code is ~w\n", [ReturnValue]),
%%     io_buffer:send_error(Buffer, Message),
%%     send_output(Buffer, ClientHandler),
%%     client_handler:finish_exec(ClientHandler, ReturnValue),
%%     process_flag(trap_exit, false),
%%     {ReturnValue, Context}.
-spec generate_process_fail_fun(ModuleDefs :: #module_defs{}) -> tuple().
generate_process_fail_fun(ModuleDefs) ->
    SetupGuard = code_generator_helper:generate_trap_guard_command(true),
    SendErrorCall = code_generator_helper:generate_io_buffer_send_error(ModuleDefs, ?MESSAGE_VAR),
    CommandFailMessage = code_generator_helper:generate_command_fail_message(?RETURN_VALUE_VAR),
    SendCommandFail = code_generator_helper:generate_io_buffer_send_error(ModuleDefs, CommandFailMessage),
    SendOutput = {call, 0, {atom, 0, ?SEND_OUTPUT_FUN}, [?BUFFER_VAR, ?CLIENT_HANDLER_VAR]},
    FinishExec = code_generator_helper:generate_finish_exec(?RETURN_VALUE_VAR, ModuleDefs),
    ReleaseGuard = code_generator_helper:generate_trap_guard_command(false),
    ReturnValue = {tuple, 0, [?RETURN_VALUE_VAR, ?CONTEXT_VAR]},
    Body = [SetupGuard, SendErrorCall, SendCommandFail, SendOutput, FinishExec, ReleaseGuard, ReturnValue],
    FunClause = {clause, 0, [?MESSAGE_VAR, ?RETURN_VALUE_VAR, ?BUFFER_VAR, ?CLIENT_HANDLER_VAR, ?CONTEXT_VAR], [], Body},
    {function, 0, ?PROCESS_FAIL_FUN, 5, [FunClause]}.