%% @author std-string

-module(code_generator).

-export([generate/4]).

-include("code_generator_defs.hrl").
-include("frame_defs.hrl").
-include("command_defs.hrl").

-define(COMMAND_EXEC_FUN, execute).
-define(PROCESS_BUFFER_FAIL_FUN, process_buffer_fail).
-define(PROCESS_COMMAND_FUN, process_command).
-define(PROCESS_SUCCESS_FUN, process_success).
-define(PROCESS_FAIL_FUN, process_fail).
-define(SEND_OUTPUT_FUN, send_output).

-define(CLI_FSM, {var, 0, 'CliFsm'}).
-define(CLIENT_HANDLER, {var, 0, 'ClientHandler'}).
-define(BUFFER, {var, 0, 'Buffer'}).
-define(CONTEXT, {var, 0, 'Context'}).
-define(MESSAGE, {var, 0, 'Message'}).
-define(RETURN_VALUE, {var, 0, 'ReturnValue'}).

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
%%             User = case lists:keyfind(user, 1, Context)) of false -> undefined; {user, Value} -> Value end,
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
%%     client_handler:finish_command(ClientHandler, 255)
%%     process_flag(trap_exit, false),
%%     {255, Context}.
%%
%% process_command(CliFsm, Buffer, ClientHandler, Context) ->
%%     case apply(CommandModule, execute, [CommandArgs, Buffer, Buffer, Context]) of
%%         {0, NewContext} ->
%%             process_success(CliFsm, Buffer, ClientHandler, NewContext);
%%         {ReturnValue, NewContext} ->
%%             Message = "Command failed with return code " ++ integer_to_list(ReturnValue),
%%             process_fail(Message, ReturnValue, Buffer, ClientHandler, NewContext)
%%     end.
%%
%% process_success(CliFsm, Buffer, ClientHandler, Context) ->
%%     process_flag(trap_exit, true),
%%     cli_fsm:process_command(CliFsm, CommandName),
%%     send_output(Buffer, ClientHandler),
%%     client_handler:finish_command(ClientHandler, 0),
%%     process_flag(trap_exit, false),
%%     {0, Context}.
%%
%% process_fail(Message, ReturnValue, Buffer, ClientHandler, Context) ->
%%     process_flag(trap_exit, true),
%%     send_output(Buffer, ClientHandler),
%%     client_handler:send_error(ClientHandler, Message),
%%     client_handler:finish_command(ClientHandler, ReturnValue),
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
    %% entry_func(CliFsm, ClientHandler, Context) ->
    %%     case io_buffer:start() of
    %%         {error, Reason} -> process_buffer_fail(ClientHandler, Context);
    %%         {ok, Buffer} ->
    %%             User = case lists:keyfind(user, 1, Context)) of false -> undefined; {user, Value} -> Value end,
    %%             case command_execution_checker:execution_precheck(CommandNam, CliFsm, dict:fetch(user, Context)) of
    %%                 {false, access_denied} -> process_fail("access denied message", 255, Buffer, ClientHandler, Context);
    %%                 {false, authorization_bad_config} -> process_fail("bad config message", 255, Buffer, ClientHandler, Context);
    %%                 {false, unsuitable_command} -> process_fail("unsuitable command message", 255, Buffer, ClientHandler, Context);
    %%                 true -> process_command(CliFsm, Buffer, ClientHandler, Context)
    %%             end
    %%     end.
    BufferStartErrorPattern = [{tuple, 0, [{atom, 0, error}, {var, 0, '_Reason'}]}],
    BufferStartErrorBody = [{call, 0, {atom, 0, ?PROCESS_BUFFER_FAIL_FUN}, [?CLIENT_HANDLER, ?CONTEXT]}],
    BufferStartErrorClause = {clause, 0, BufferStartErrorPattern, [], BufferStartErrorBody},
    BufferStartSuccessPattern = [{tuple, 0, [{atom, 0, ok}, ?BUFFER]}],
    BufferStartSuccessBody = [generate_precheck_command(CommandName, ModuleDefs)],
    BufferStartSuccessClause = {clause, 0, BufferStartSuccessPattern, [], BufferStartSuccessBody},
    CaseExpr = {call, 0, {remote, 0, {atom, 0, ModuleDefs#module_defs.io_buffer_module}, {atom, 0, start}}, []},
    Body = [{'case', 0, CaseExpr, [BufferStartErrorClause, BufferStartSuccessClause]}],
    FunClause = {clause, 0, [?CLI_FSM, ?CLIENT_HANDLER, ?CONTEXT], [], Body},
    {function, 0, EntryFunName, 3, [FunClause]}.

-spec generate_precheck_command(CommandName :: atom(), ModuleDefs :: #module_defs{}) -> tuple().
generate_precheck_command(CommandName, ModuleDefs) ->
    %% User = case lists:keyfind(user, 1, Context)) of false -> undefined; {user, Value} -> Value end,
    %% case command_execution_checker:execution_precheck(CommandName, CliFsm, dict:fetch(user, Context)) of
    %%     {false, access_denied} -> process_fail("access denied message", 255, Buffer, ClientHandler, Context);
    %%     {false, authorization_bad_config} -> process_fail("bad config message", 255, Buffer, ClientHandler, Context);
    %%     {false, unsuitable_command} -> process_fail("unsuitable command message", 255, Buffer, ClientHandler, Context);
    %%     true -> process_command(CliFsm, Buffer, ClientHandler, Context)
    %% end
    AccessDeniedPattern = [{tuple, 0, [{atom, 0, false}, {atom, 0, access_denied}]}],
    AccessDeniedProcessArgs = [{string, 0, ?ACCESS_DENIED_MESSAGE}, {integer, 0, 255}, ?BUFFER, ?CLIENT_HANDLER, ?CONTEXT],
    AccessDeniedBody = [{call, 0, {atom, 0, ?PROCESS_FAIL_FUN}, AccessDeniedProcessArgs}],
    AccessDeniedClause = {clause, 0, AccessDeniedPattern, [], AccessDeniedBody},
    BadConfigPattern = [{tuple, 0, [{atom, 0, false}, {atom, 0, authorization_bad_config}]}],
    BadConfigProcessArgs = [{string, 0, ?BAD_CONFIG_MESSAGE}, {integer, 0, 255}, ?BUFFER, ?CLIENT_HANDLER, ?CONTEXT],
    BadConfigBody = [{call, 0, {atom, 0, ?PROCESS_FAIL_FUN}, BadConfigProcessArgs}],
    BadConfigClause = {clause, 0, BadConfigPattern, [], BadConfigBody},
    UnsuitableCommandPattern = [{tuple, 0, [{atom, 0, false}, {atom, 0, unsuitable_command}]}],
    UnsuitableCommandProcessArgs = [{string, 0, ?UNSUITABLE_COMMAND_MESSAGE}, {integer, 0, 255}, ?BUFFER, ?CLIENT_HANDLER, ?CONTEXT],
    UnsuitableCommandBody = [{call, 0, {atom, 0, ?PROCESS_FAIL_FUN}, UnsuitableCommandProcessArgs}],
    UnsuitableCommandClause = {clause, 0, UnsuitableCommandPattern, [], UnsuitableCommandBody},
    SuccessBody = [{call, 0, {atom, 0, ?PROCESS_COMMAND_FUN}, [?CLI_FSM, ?BUFFER, ?CLIENT_HANDLER, ?CONTEXT]}],
    SuccessClause = {clause, 0, [{atom, 0, true}], [], SuccessBody},
    User = generate_find_user_command(),
    CaseExprArgs = [{atom, 0, CommandName}, ?CLI_FSM, User],
    CaseExpr = {call, 0, {remote, 0, {atom, 0, ModuleDefs#module_defs.exec_checker_module}, {atom, 0, execution_precheck}}, CaseExprArgs},
    {'case', 0, CaseExpr, [AccessDeniedClause, BadConfigClause, UnsuitableCommandClause, SuccessClause]}.

-spec generate_find_user_command() -> tuple().
generate_find_user_command() ->
    %% User = case lists:keyfind(user, 1, Context)) of false -> undefined; {user, Value} -> Value end,
    CaseExpr = {call, 0, {remote, 0, {atom, 0, lists}, {atom, 0, keyfind}}, [{atom, 0, ?USER_KEY}, {integer, 0, 1}, ?CONTEXT]},
    NotFoundClause = {clause, 0, [{atom, 0, false}], [], [{atom, 0, undefined}]},
    FoundPattern = [{tuple, 0, [{atom, 0, ?USER_KEY}, {var, 0, 'Value'}]}],
    FoundClause = {clause, 0, FoundPattern, [], [{var, 0, 'Value'}]},
    {'case', 0, CaseExpr, [NotFoundClause, FoundClause]}.

-spec generate_process_command_fun(Command :: #command{}) -> tuple().
generate_process_command_fun(#command{module = Module, arguments =Args}) ->
    %% process_command(CliFsm, Buffer, ClientHandler, Context) ->
    %%     case apply(CommandModule, execute, [CommandArgs, Buffer, Buffer, Context]) of
    %%         {0, NewContext} ->
    %%             process_success(CliFsm, Buffer, ClientHandler, NewContext);
    %%         {ReturnValue, NewContext} ->
    %%             Message = "Command failed with return code " ++ integer_to_list(ReturnValue),
    %%             process_fail(Message, ReturnValue, Buffer, ClientHandler, NewContext)
    %%     end.
    CommandArgs = [generate_arg_list(Args), ?BUFFER, ?BUFFER, ?CONTEXT],
    CaseExpr = {call, 0, {remote, 0, {atom, 0, Module}, {atom, 0, ?COMMAND_EXEC_FUN}}, CommandArgs},
    SuccessBody = [{call, 0, {atom, 0, ?PROCESS_SUCCESS_FUN}, [?CLI_FSM, ?BUFFER, ?CLIENT_HANDLER, {var, 0, 'NewContext'}]}],
    SuccessPattern = [{tuple, 0, [{atom, 0, 0}, {var, 0, 'NewContext'}]}],
    SuccessClause = {clause, 0, SuccessPattern, [], SuccessBody},
    Message = generate_command_fail_message_command(),
    ProcessFailArgs = [Message, ?RETURN_VALUE, ?BUFFER, ?CLIENT_HANDLER, {var, 0, 'NewContext'}],
    FailBody = [{call, 0, {atom, 0, ?PROCESS_FAIL_FUN}, ProcessFailArgs}],
    FailPattern = [{tuple, 0, [?RETURN_VALUE, {var, 0, 'NewContext'}]}],
    FailClause = {clause, 0, FailPattern, [], FailBody},
    Body = [{'case', 0, CaseExpr, [SuccessClause, FailClause]}],
    FunClause = {clause, 0, [?CLI_FSM, ?BUFFER, ?CLIENT_HANDLER, ?CONTEXT], [], Body},
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

-spec generate_command_fail_message_command() -> tuple().
generate_command_fail_message_command() ->
    %% Message = "Command failed with return code " ++ integer_to_list(ReturnValue)
    ReturnCodeStr = {call, 0, {remote, 0, {atom, 0, erlang}, {atom, 0, integer_to_list}}, [?RETURN_VALUE]},
    {op, 0, '++', {string, 0, ?COMMAND_FAIL_MESSAGE}, ReturnCodeStr}.


-spec generate_process_success_fun(CommandName :: atom(), ModuleDefs :: #module_defs{}) -> tuple().
generate_process_success_fun(CommandName, ModuleDefs) ->
    %% process_success(CliFsm, Buffer, ClientHandler, Context) ->
    %%     process_flag(trap_exit, true),
    %%     cli_fsm:process_command(CliFsm, CommandName),
    %%     send_output(Buffer, ClientHandler),
    %%     client_handler:finish_command(ClientHandler, 0),
    %%     process_flag(trap_exit, false),
    %%     {0, Context}.
    SetupGuard = generate_trap_guard_command(true),
    FsmNotification = generate_fsm_notification_command(CommandName, ModuleDefs),
    SendOutput = {call, 0, {atom, 0, ?SEND_OUTPUT_FUN}, [?BUFFER, ?CLIENT_HANDLER]},
    FinishCommand = generate_finish_command({integer, 0, 0}, ModuleDefs),
    ReleaseGuard = generate_trap_guard_command(false),
    ReturnValue = {tuple, 0, [{integer, 0, 0}, ?CONTEXT]},
    Body = [SetupGuard, FsmNotification, SendOutput, FinishCommand, ReleaseGuard, ReturnValue],
    FunClause = {clause, 0, [?CLI_FSM, ?BUFFER, ?CLIENT_HANDLER, ?CONTEXT], [], Body},
    {function, 0, ?PROCESS_SUCCESS_FUN, 4, [FunClause]}.

-spec generate_process_buffer_fail_fun(ModuleDefs :: #module_defs{}) -> tuple().
generate_process_buffer_fail_fun(ModuleDefs) ->
    %% process_buffer_fail(ClientHandler, Context) ->
    %%     process_flag(trap_exit, true),
    %%     client_handler:send_error(ClientHandler, "buffer initialization message"),
    %%     client_handler:finish_command(ClientHandler, 255)
    %%     process_flag(trap_exit, false),
    %%     {255, Context}.
    SetupGuard = generate_trap_guard_command(true),
    SendErrorCall = generate_send_error_command(ModuleDefs, {string, 0, ?BUFFER_START_FAIL_MESSAGE}),
    FinishCommand = generate_finish_command({integer, 0, 255}, ModuleDefs),
    ReleaseGuard = generate_trap_guard_command(false),
    ReturnValue = {tuple, 0, [{integer, 0, 255}, ?CONTEXT]},
    Body = [SetupGuard, SendErrorCall, FinishCommand, ReleaseGuard, ReturnValue],
    FunClause = {clause, 0, [?CLIENT_HANDLER, ?CONTEXT], [], Body},
    {function, 0, ?PROCESS_BUFFER_FAIL_FUN, 2, [FunClause]}.

-spec generate_process_fail_fun(ModuleDefs :: #module_defs{}) -> tuple().
generate_process_fail_fun(ModuleDefs) ->
    %% process_fail(Message, ReturnValue, Buffer, ClientHandler, Context) ->
    %%     process_flag(trap_exit, true),
    %%     send_output(Buffer, ClientHandler),
    %%     client_handler:send_error(ClientHandler, Message),
    %%     client_handler:finish_command(ClientHandler, ReturnValue),
    %%     process_flag(trap_exit, false),
    %%     {ReturnValue, Context}.
    SetupGuard = generate_trap_guard_command(true),
    SendOutput = {call, 0, {atom, 0, ?SEND_OUTPUT_FUN}, [?BUFFER, ?CLIENT_HANDLER]},
    SendErrorCall = generate_send_error_command(ModuleDefs),
    FinishCommand = generate_finish_command(?RETURN_VALUE, ModuleDefs),
    ReleaseGuard = generate_trap_guard_command(false),
    ReturnValue = {tuple, 0, [?RETURN_VALUE, ?CONTEXT]},
    Body = [SetupGuard, SendOutput, SendErrorCall, FinishCommand, ReleaseGuard, ReturnValue],
    FunClause = {clause, 0, [?MESSAGE, ?RETURN_VALUE, ?BUFFER, ?CLIENT_HANDLER, ?CONTEXT], [], Body},
    {function, 0, ?PROCESS_FAIL_FUN, 5, [FunClause]}.

-spec generate_fsm_notification_command(CommandName :: atom(), ModuleDefs :: #module_defs{}) -> tuple().
generate_fsm_notification_command(CommandName, ModuleDefs) ->
    %% cli_fsm:process_command(CliFsm, CommandName),
    NotificationArgs = [{atom, 0, CommandName}, {var, 0, 'CliFsm'}],
    {call, 0, {remote, 0, {atom, 0, ModuleDefs#module_defs.cli_fsm_module}, {atom, 0, process_command}}, NotificationArgs}.

-spec generate_send_error_command(ModuleDefs :: #module_defs{}) -> tuple().
generate_send_error_command(ModuleDefs) ->
    %% client_handler:send_error(ClientHandler, Message),
    generate_send_error_command(ModuleDefs, ?MESSAGE).

-spec generate_send_error_command(ModuleDefs :: #module_defs{}, Message :: tuple()) -> tuple().
generate_send_error_command(ModuleDefs, Message) ->
    %% client_handler:send_error(ClientHandler, Message),
    SendErrorCallArgs = [?CLIENT_HANDLER, Message],
    {call, 0, {remote, 0, {atom, 0, ModuleDefs#module_defs.client_handler_module}, {atom, 0, send_error}}, SendErrorCallArgs}.

-spec generate_send_output_fun(ModuleDefs :: #module_defs{}) -> tuple().
generate_send_output_fun(ModuleDefs) ->
    %% send_output(Buffer, ClientHandler) ->
    %%     ProcessFun = fun({output, Message}) -> client_handler:send_output(ClientHandler, Message) end;
    %%                  fun({error, Message}) -> client_handler:send_error(ClientHandler, Message) end,
    %%     Data = io_buffer:get_data(Buffer, both),
    %%     lists:foreach(ProcessFun, Data).
    DataRetrieveArgs = [?BUFFER, {atom, 0, both}],
    DataRetrieve = {call, 0, {remote, 0, {atom, 0, ModuleDefs#module_defs.io_buffer_module}, {atom, 0, get_data}}, DataRetrieveArgs},
    DataDef = {match, 0, {var, 0, 'Data'}, DataRetrieve},
    SendOutputPattern = [{tuple, 0, [{atom, 0, output}, ?MESSAGE]}],
    SendOutputCallArgs = [?CLIENT_HANDLER, ?MESSAGE],
    SendOutputCall = {call, 0, {remote, 0, {atom, 0, ModuleDefs#module_defs.client_handler_module}, {atom, 0, send_output}}, SendOutputCallArgs},
    SendOutputClause = {clause, 0, SendOutputPattern, [], [SendOutputCall]},
    SendErrorPattern = [{tuple, 0, [{atom, 0, error}, ?MESSAGE]}],
    SendErrorCallArgs = [?CLIENT_HANDLER, ?MESSAGE],
    SendErrorCall = {call, 0, {remote, 0, {atom, 0, ModuleDefs#module_defs.client_handler_module}, {atom, 0, send_error}}, SendErrorCallArgs},
    SendErrorClause = {clause, 0, SendErrorPattern, [], [SendErrorCall]},
    ProcessFun = {'fun', 0, {clauses, [SendOutputClause, SendErrorClause]}},
    ForeachArgs = [ProcessFun, {var, 0, 'Data'}],
    Foreach = {call, 0, {remote, 0, {atom, 0, lists}, {atom, 0, foreach}}, ForeachArgs},
    Body = [DataDef, Foreach],
    FunClause = {clause, 0, [?BUFFER, ?CLIENT_HANDLER], [], Body},
    {function, 0, ?SEND_OUTPUT_FUN, 2, [FunClause]}.

-spec generate_finish_command(ReturnValue :: tuple(), ModuleDefs :: #module_defs{}) -> tuple().
generate_finish_command(ReturnValue, ModuleDefs) ->
    %% client_handler:finish_command(ClientHandler, ReturnValue),
    FinishCommandArgs = [?CLIENT_HANDLER, ReturnValue],
    {call, 0, {remote, 0, {atom, 0, ModuleDefs#module_defs.client_handler_module}, {atom, 0, finish_command}}, FinishCommandArgs}.

-spec generate_trap_guard_command(TrapExit :: boolean) -> tuple().
generate_trap_guard_command(TrapExit) ->
    %% process_flag(trap_exit, TrapExit)
    GuardArgs = [{atom, 0, trap_exit}, {atom, 0, TrapExit}],
    {call, 0, {remote, 0, {atom, 0, erlang}, {atom, 0, process_flag}}, GuardArgs}.