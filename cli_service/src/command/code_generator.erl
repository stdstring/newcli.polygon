%% @author std-string

-module(code_generator).

-export([generate/4]).

-include("code_generator_defs.hrl").
-include("frame_defs.hrl").

-define(PROCESS_BUFFER_FAIL_FUN, process_buffer_fail).
-define(PROCESS_COMMAND_FUN, process_command).
-define(PROCESS_SUCCESS_FUN, process_success).
-define(PROCESS_FAIL_FUN, process_fail).
-define(SEND_OUTPUT_FUN, send_output).

%% ====================================================================
%% API functions
%% ====================================================================

%% expectation in pseudocode :
%%
%% CommandName = ...
%% CommandModule = ...
%% CommandArgs = ...
%% entry_func(CliFsm, ClientHandler, User) ->
%%     case io_buffer:start() of
%%         {error, Reason} -> process_buffer_fail(ClientHandler);
%%         {ok, Buffer} ->
%%             case command_execution_checker:execution_precheck(CommandNam, CliFsm, User) of
%%                 {false, access_denied} -> process_fail("access denied message", 255, Buffer, ClientHandler);
%%                 {false, authorization_bad_config} -> process_fail("bad config message", 255, Buffer, ClientHandler);
%%                 {false, unsuitable_command} -> process_fail("unsuitable command message", 255, Buffer, ClientHandler);
%%                 true -> process_command(CliFsm, Buffer, ClientHandler)
%%             end
%%     end.
%%
%% process_buffer_fail(ClientHandler) ->
%%     process_flag(trap_exit, true),
%%     client_handler:send_error(ClientHandler, "buffer initialization message"),
%%     client_handler:finish_command(ClientHandler, 255)
%%     process_flag(trap_exit, false).
%%
%% process_command(CliFsm, Buffer, ClientHandler) ->
%%     case apply(CommandModule, execute, [Buffer, Buffer, CommandArgs]) of
%%         0 ->
%%             process_success(CliFsm, Buffer, ClientHandler);
%%         ReturnCode ->
%%             Message = "Command failed with return code " ++ integer_to_list(ReturnCode),
%%             process_fail(Message, ReturnCode, Buffer, ClientHandler)
%%     end.
%%
%% process_success(CliFsm, Buffer, ClientHandler) ->
%%     process_flag(trap_exit, true),
%%     cli_fsm:process_command(CliFsm, CommandName),
%%     send_output(Buffer, ClientHandler),
%%     client_handler:finish_command(ClientHandler, 0),
%%     process_flag(trap_exit, false).
%%
%% process_fail(Message, ReturnCode, Buffer, ClientHandler) ->
%%     process_flag(trap_exit, true),
%%     send_output(Buffer, ClientHandler),
%%     client_handler:send_error(ClientHandler, Message),
%%     client_handler:finish_command(ClientHandler, ReturnCode),
%%     process_flag(trap_exit, false).
%%
%% send_output(Buffer, ClientHandler) ->
%%     ProcessFun = fun({output, Message}) -> client_handler:send_output(ClientHandler, Message) end;
%%                  fun({error, Message}) -> client_handler:send_error(ClientHandler, Message) end,
%%     Data = io_buffer:get_data(Buffer, both),
%%     lists:foreach(ProcessFun, Data).


-spec generate(EntryModuleName :: atom(), EntryFunName :: atom(), Command :: #command{}, ModuleDefs :: #module_defs{}) ->
    {'true', Binary :: binary()} | {'false', Reason :: term()}.
generate(EntryModuleName, EntryFunName, Command, ModuleDefs) ->
    CommandName = apply(Command#command.module, get_name, []),
    ModuleForm = {attribute, 0, module, EntryModuleName},
    ExportForm = {attribute, 0, export, [{EntryFunName, 3}]},
    EntryFun = generate_entry_func(CommandName, ModuleDefs, EntryFunName),
    ProcessFun = generate_process_command_fun(Command),
    ProcessSuccessFun = generate_process_success_fun(CommandName, ModuleDefs),
    BufferFailFun = generate_process_buffer_fail_fun(ModuleDefs),
    ProcessFailFun = generate_process_fail_fun(ModuleDefs),
    SendOutputFun = generate_send_output_fun(ModuleDefs),
    CompileResult = compile:forms([ModuleForm, ExportForm, EntryFun, ProcessFun, ProcessSuccessFun, BufferFailFun, ProcessFailFun, SendOutputFun]),
    case CompileResult of
        {ok, EntryModuleName, Binary} -> {true, Binary};
        _Other -> {false, undefined}
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec generate_entry_func(CommandName :: atom(), ModuleDefs :: #module_defs{}, EntryFunName :: atom()) -> tuple().
generate_entry_func(CommandName, ModuleDefs, EntryFunName) ->
    %% entry_func(CliFsm, ClientHandler, User) ->
    %%     case io_buffer:start() of
    %%         {error, Reason} -> process_buffer_fail(ClientHandler);
    %%         {ok, Buffer} ->
    %%             case command_execution_checker:execution_precheck(CommandNam, CliFsm, User) of
    %%                 {false, access_denied} -> process_fail("access denied message", 255, Buffer, ClientHandler);
    %%                 {false, authorization_bad_config} -> process_fail("bad config message", 255, Buffer, ClientHandler);
    %%                 {false, unsuitable_command} -> process_fail("unsuitable command message", 255, Buffer, ClientHandler);
    %%                 true -> process_command(CliFsm, Buffer, ClientHandler)
    %%             end
    %%     end.
    BufferStartErrorPattern = [{tuple, 0, [{atom, 0, error}, {var, 0, '_Reason'}]}],
    BufferStartErrorBody = [{call, 0, {atom, 0, ?PROCESS_BUFFER_FAIL_FUN}, [{var, 0, 'ClientHandler'}]}],
    BufferStartErrorClause = {clause, 0, BufferStartErrorPattern, [], BufferStartErrorBody},
    BufferStartSuccessPattern = [{tuple, 0, [{atom, 0, ok}, {var, 0, 'Buffer'}]}],
    BufferStartSuccessBody = [generate_precheck_command(CommandName, ModuleDefs)],
    BufferStartSuccessClause = {clause, 0, BufferStartSuccessPattern, [], BufferStartSuccessBody},
    CaseExpr = {call, 0, {remote, 0, {atom, 0, ModuleDefs#module_defs.io_buffer_module}, {atom, 0, start}}, []},
    Body = [{'case', 0, CaseExpr, [BufferStartErrorClause, BufferStartSuccessClause]}],
    FunClause = {clause, 0, [{var, 0, 'CliFsm'}, {var, 0, 'ClientHandler'}, {var, 0, 'User'}], [], Body},
    {function, 0, EntryFunName, 3, [FunClause]}.

-spec generate_precheck_command(CommandName :: atom(), ModuleDefs :: #module_defs{}) -> tuple().
generate_precheck_command(CommandName, ModuleDefs) ->
    %% case command_execution_checker:execution_precheck(CommandNam, CliFsm, User) of
    %%     {false, access_denied} -> process_fail("access denied message", 255, Buffer, ClientHandler);
    %%     {false, authorization_bad_config} -> process_fail("bad config message", 255, Buffer, ClientHandler);
    %%     {false, unsuitable_command} -> process_fail("unsuitable command message", 255, Buffer, ClientHandler);
    %%     true -> process_command(CliFsm, Buffer, ClientHandler)
    %% end
    AccessDeniedPattern = [{tuple, 0, [{atom, 0, false}, {atom, 0, access_denied}]}],
    AccessDeniedProcessArgs = [{string, 0, ?ACCESS_DENIED_MESSAGE}, {integer, 0, 255}, {var, 0, 'Buffer'}, {var, 0, 'ClientHandler'}],
    AccessDeniedBody = [{call, 0, {atom, 0, ?PROCESS_FAIL_FUN}, AccessDeniedProcessArgs}],
    AccessDeniedClause = {clause, 0, AccessDeniedPattern, [], AccessDeniedBody},
    BadConfigPattern = [{tuple, 0, [{atom, 0, false}, {atom, 0, authorization_bad_config}]}],
    BadConfigProcessArgs = [{string, 0, ?BAD_CONFIG_MESSAGE}, {integer, 0, 255}, {var, 0, 'Buffer'}, {var, 0, 'ClientHandler'}],
    BadConfigBody = [{call, 0, {atom, 0, ?PROCESS_FAIL_FUN}, BadConfigProcessArgs}],
    BadConfigClause = {clause, 0, BadConfigPattern, [], BadConfigBody},
    UnsuitableCommandPattern = [{tuple, 0, [{atom, 0, false}, {atom, 0, unsuitable_command}]}],
    UnsuitableCommandProcessArgs = [{string, 0, ?UNSUITABLE_COMMAND_MESSAGE}, {integer, 0, 255}, {var, 0, 'Buffer'}, {var, 0, 'ClientHandler'}],
    UnsuitableCommandBody = [{call, 0, {atom, 0, ?PROCESS_FAIL_FUN}, UnsuitableCommandProcessArgs}],
    UnsuitableCommandClause = {clause, 0, UnsuitableCommandPattern, [], UnsuitableCommandBody},
    SuccessBody = [{call, 0, {atom, 0, ?PROCESS_COMMAND_FUN}, [{var, 0, 'CliFsm'}, {var, 0, 'Buffer'}, {var, 0, 'ClientHandler'}]}],
    SuccessClause = {clause, 0, [{atom, 0, true}], [], SuccessBody},
    CaseExprArgs = [{atom, 0, CommandName}, {var, 0, 'CliFsm'}, {var, 0, 'User'}],
    CaseExpr = {call, 0, {remote, 0, {atom, 0, ModuleDefs#module_defs.exec_checker_module}, {atom, 0, execution_precheck}}, CaseExprArgs},
    {'case', 0, CaseExpr, [AccessDeniedClause, BadConfigClause, UnsuitableCommandClause, SuccessClause]}.

-spec generate_process_command_fun(Command :: #command{}) -> tuple().
generate_process_command_fun(#command{module = Module, function = Func, arguments =Args}) ->
    %% process_command(CliFsm, Buffer, ClientHandler) ->
    %%     case apply(CommandModule, execute, [Buffer, Buffer, CommandArgs]) of
    %%         0 ->
    %%             process_success(CliFsm, Buffer, ClientHandler);
    %%         ReturnCode ->
    %%             Message = "Command failed with return code " ++ ReturnCode,
    %%             process_fail(Message, ReturnCode, Buffer, ClientHandler)
    %%     end.
    ArgsList = generate_arg_list(Args),
    CaseExpr = {call, 0, {remote, 0, {atom, 0, Module}, {atom, 0, Func}}, [ArgsList]},
    SuccessBody = [{call, 0, {atom, 0, ?PROCESS_SUCCESS_FUN}, [{var, 0, 'CliFsm'}, {var, 0, 'Buffer'}, {var, 0, 'ClientHandler'}]}],
    SuccessClause = {clause, 0, [{atom, 0, 0}], [], SuccessBody},
    Message = generate_command_fail_message_command(),
    ProcessFailArgs = [Message, {var, 0, 'ReturnCode'}, {var, 0, 'Buffer'}, {var, 0, 'ClientHandler'}],
    FailBody = [{call, 0, {atom, 0, ?PROCESS_FAIL_FUN}, ProcessFailArgs}],
    FailClause = {clause, 0, [{var, 0, 'ReturnCode'}], [], FailBody},
    Body = [{'case', 0, CaseExpr, [SuccessClause, FailClause]}],
    FunClause = {clause, 0, [{var, 0, 'CliFsm'}, {var, 0, 'Buffer'}, {var, 0, 'ClientHandler'}], [], Body},
    {function, 0, ?PROCESS_COMMAND_FUN, 3, [FunClause]}.

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

-spec generate_command_fail_message_command() -> tuple().
generate_command_fail_message_command() ->
    %% Message = "Command failed with return code " ++ integer_to_list(ReturnCode)
    ReturnCodeStr = {call, 0, {remote, 0, {atom, 0, erlang}, {atom, 0, integer_to_list}}, [{var, 0, 'ReturnCode'}]},
    {op, 0, '++', {string, 0, ?COMMAND_FAIL_MESSAGE}, ReturnCodeStr}.


-spec generate_process_success_fun(CommandName :: atom(), ModuleDefs :: #module_defs{}) -> tuple().
generate_process_success_fun(CommandName, ModuleDefs) ->
    %% process_success(CliFsm, Buffer, ClientHandler) ->
    %%     process_flag(trap_exit, true),
    %%     cli_fsm:process_command(CliFsm, CommandName),
    %%     send_output(Buffer, ClientHandler),
    %%     client_handler:finish_command(ClientHandler, 0),
    %%     process_flag(trap_exit, false).
    SetupGuard = generate_trap_guard_command(true),
    FsmNotification = generate_fsm_notification_command(CommandName, ModuleDefs),
    SendOutput = {call, 0, {atom, 0, ?SEND_OUTPUT_FUN}, [{var, 0, 'Buffer'}, {var, 0, 'ClientHandler'}]},
    FinishCommand = generate_finish_command({integer, 0, 0}, ModuleDefs),
    ReleaseGuard = generate_trap_guard_command(false),
    Body = [SetupGuard, FsmNotification, SendOutput, FinishCommand, ReleaseGuard],
    FunClause = {clause, 0, [{var, 0, 'CliFsm'}, {var, 0, 'Buffer'}, {var, 0, 'ClientHandler'}], [], Body},
    {function, 0, ?PROCESS_SUCCESS_FUN, 3, [FunClause]}.

-spec generate_process_buffer_fail_fun(ModuleDefs :: #module_defs{}) -> tuple().
generate_process_buffer_fail_fun(ModuleDefs) ->
    %% process_buffer_fail(ClientHandler) ->
    %%     process_flag(trap_exit, true),
    %%     client_handler:send_error(ClientHandler, "buffer initialization message"),
    %%     client_handler:finish_command(ClientHandler, 255)
    %%     process_flag(trap_exit, false).
    SetupGuard = generate_trap_guard_command(true),
    SendErrorCall = generate_send_error_command(ModuleDefs, {string, 0, ?BUFFER_START_FAIL_MESSAGE}),
    FinishCommand = generate_finish_command({integer, 0, 255}, ModuleDefs),
    ReleaseGuard = generate_trap_guard_command(false),
    Body = [SetupGuard, SendErrorCall, FinishCommand, ReleaseGuard],
    FunClause = {clause, 0, [{var, 0, 'ClientHandler'}], [], Body},
    {function, 0, ?PROCESS_BUFFER_FAIL_FUN, 1, [FunClause]}.

-spec generate_process_fail_fun(ModuleDefs :: #module_defs{}) -> tuple().
generate_process_fail_fun(ModuleDefs) ->
    %% process_fail(Message, ReturnCode, Buffer, ClientHandler) ->
    %%     process_flag(trap_exit, true),
    %%     send_output(Buffer, ClientHandler)
    %%     client_handler:send_error(ClientHandler, Message),
    %%     client_handler:finish_command(ClientHandler, ReturnValue)
    %%     process_flag(trap_exit, false).
    SetupGuard = generate_trap_guard_command(true),
    SendOutput = {call, 0, {atom, 0, ?SEND_OUTPUT_FUN}, [{var, 0, 'Buffer'}, {var, 0, 'ClientHandler'}]},
    SendErrorCall = generate_send_error_command(ModuleDefs),
    FinishCommand = generate_finish_command({var, 0, 'ReturnValue'}, ModuleDefs),
    ReleaseGuard = generate_trap_guard_command(false),
    Body = [SetupGuard, SendOutput, SendErrorCall, FinishCommand, ReleaseGuard],
    FunClause = {clause, 0, [{var, 0, 'Message'}, {var, 0, 'ReturnValue'}, {var, 0, 'Buffer'}, {var, 0, 'ClientHandler'}], [], Body},
    {function, 0, ?PROCESS_FAIL_FUN, 4, [FunClause]}.

-spec generate_fsm_notification_command(CommandName :: atom(), ModuleDefs :: #module_defs{}) -> tuple().
generate_fsm_notification_command(CommandName, ModuleDefs) ->
    %% cli_fsm:process_command(CliFsm, CommandName),
    NotificationArgs = [{atom, 0, CommandName}, {var, 0, 'CliFsm'}],
    {call, 0, {remote, 0, {atom, 0, ModuleDefs#module_defs.cli_fsm_module}, {atom, 0, process_command}}, NotificationArgs}.

-spec generate_send_error_command(ModuleDefs :: #module_defs{}) -> tuple().
generate_send_error_command(ModuleDefs) ->
    %% client_handler:send_error(ClientHandler, Message),
    generate_send_error_command(ModuleDefs, {var, 0, 'Message'}).

-spec generate_send_error_command(ModuleDefs :: #module_defs{}, Message :: tuple()) -> tuple().
generate_send_error_command(ModuleDefs, Message) ->
    %% client_handler:send_error(ClientHandler, Message),
    SendErrorCallArgs = [{var, 0, 'ClientHandler'}, Message],
    {call, 0, {remote, 0, {atom, 0, ModuleDefs#module_defs.client_handler_module}, {atom, 0, send_error}}, SendErrorCallArgs}.

-spec generate_send_output_fun(ModuleDefs :: #module_defs{}) -> tuple().
generate_send_output_fun(ModuleDefs) ->
    %% send_output(Buffer, ClientHandler) ->
    %%     ProcessFun = fun({output, Message}) -> client_handler:send_output(ClientHandler, Message) end;
    %%                  fun({error, Message}) -> client_handler:send_error(ClientHandler, Message) end,
    %%     Data = io_buffer:get_data(Buffer, both),
    %%     lists:foreach(ProcessFun, Data).
    DataRetrieveArgs = [{var, 0, 'Buffer'}, {atom, 0, both}],
    DataRetrieve = {call, 0, {remote, 0, {atom, 0, ModuleDefs#module_defs.io_buffer_module}, {atom, 0, get_data}}, DataRetrieveArgs},
    DataDef = {match, 0, {var, 0, 'Data'}, DataRetrieve},
    SendOutputPattern = [{tuple, 0, [{atom, 0, output}, {var, 0, 'Message'}]}],
    SendOutputCallArgs = [{var, 0, 'ClientHandler'}, {var, 0, 'Message'}],
    SendOutputCall = {call, 0, {remote, 0, {atom, 0, ModuleDefs#module_defs.client_handler_module}, {atom, 0, send_output}}, SendOutputCallArgs},
    SendOutputClause = {clause, 0, SendOutputPattern, [], [SendOutputCall]},
    SendErrorPattern = [{tuple, 0, [{atom, 0, error}, {var, 0, 'Message'}]}],
    SendErrorCallArgs = [{var, 0, 'ClientHandler'}, {var, 0, 'Message'}],
    SendErrorCall = {call, 0, {remote, 0, {atom, 0, ModuleDefs#module_defs.client_handler_module}, {atom, 0, send_error}}, SendErrorCallArgs},
    SendErrorClause = {clause, 0, SendErrorPattern, [], [SendErrorCall]},
    ProcessFun = {'fun', 0, {clauses, [SendOutputClause, SendErrorClause]}},
    ForeachArgs = [ProcessFun, {var, 0, 'Data'}],
    Foreach = {call, 0, {remote, 0, {atom, 0, lists}, {atom, 0, foreach}}, ForeachArgs},
    Body = [DataDef, Foreach],
    FunClause = {clause, 0, [{var, 0, 'Buffer'}, {var, 0, 'ClientHandler'}], [], Body},
    {function, 0, ?SEND_OUTPUT_FUN, 2, [FunClause]}.

-spec generate_finish_command(ReturnValue :: tuple(), ModuleDefs :: #module_defs{}) -> tuple().
generate_finish_command(ReturnValue, ModuleDefs) ->
    %% client_handler:finish_command(ClientHandler, ReturnValue),
    FinishCommandArgs = [{var, 0, 'ClientHandler'}, ReturnValue],
    {call, 0, {remote, 0, {atom, 0, ModuleDefs#module_defs.client_handler_module}, {atom, 0, finish_command}}, FinishCommandArgs}.

-spec generate_trap_guard_command(TrapExit :: boolean) -> tuple().
generate_trap_guard_command(TrapExit) ->
    %% process_flag(trap_exit, TrapExit)
    GuardArgs = [{atom, 0, trap_exit}, {atom, 0, TrapExit}],
    {call, 0, {remote, 0, {atom, 0, erlang}, {atom, 0, process_flag}}, GuardArgs}.