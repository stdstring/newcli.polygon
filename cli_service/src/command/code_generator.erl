%% @author std-string

-module(code_generator).

-export([generate/3]).
%% temporary export
-export([generate_before_command/0,
         generate_command_execution/1,
         generate_success_finalizator/1]).

-include("frame_defs.hrl").

-define(SUCCESS_FIN_NAME, success_finalizator).
-define(FAIL_FIN_NAME, fail_finalizator).
%% TODO (std_string) : definition of the error message must 
-define(UNSUITABLE_COMMAND_MESSAGE, "").

%% ====================================================================
%% API functions
%% ====================================================================

-spec generate(EntryModule :: atom(), EntryFunc :: atom(), Command :: #command{}) -> 'ok'.
generate(EntryModule, EntryFunc, Command) ->
    %%Body = [generate_before_command(), generate_command_execution(Command), generate_after_command()],
    Body = [],
    Clause = {clause, 0, [], [], Body},
    ModuleForm = {attribute, 0, module, EntryModule},
    ExportForm = {attribute, 0, export, [{EntryFunc, 0}]},
    FunForm = {function, 0, EntryFunc, 0, [Clause]},
    case compile:forms([ModuleForm, ExportForm, FunForm]) of
        {ok, EntryModule, Binary} -> {ok, Binary};
        _Other -> {false, undefined}
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec generate_before_command() -> tuple().
generate_before_command(CommandName) ->
    %% before_checker(CliFsm, User, ClientHandler) -> ReturnValue :: integer()
    SuccessBody = [{integer, 0, 0}],
    SuccessClause = {clause, 0, [{atom, 0, true}], [], Body},
    UnsuitableCommandPattern = [{tuple, 0, [{atom, 0, false}, {atom, 0, unsuitable_command}]}],
    UnsuitableCommandBody = [{integer, 0, 0}],
    UnsuitableCommandClause = {clause, 0, UnsuitableCommandPattern, [], Body},
    {}.

-spec generate_command_execution(Command :: #command{}) -> tuple().
generate_command_execution(#command{module = Module, function = Func, arguments =Args}) ->
    ArgsList = generate_arg_list(Args),
    {call, 0, {remote, 0, {atom, 0, Module}, {atom, 0, Func}}, [ArgsList]}.

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

-spec generate_success_finalizator(CommandName :: atom()) -> tuple().
generate_success_finalizator(CommandName) ->
    %% success_finalizator(CliFsm, Buffer, ClientHandler) -> ok
    SetupGuard = generate_trap_guard(true),
    SendOutput = generate_send_output(),
    FinishCommand = generate_finish_command({integer, 0, 0}),
    FsmNotification = generate_fsm_notification(CommandName),
    ReleaseGuard = generate_trap_guard(false),
    Body = [SetupGuard, SendOutput, FinishCommand, FsmNotification, ReleaseGuard],
    FunClause = {clause, 0, [{var, 0, 'CliFsm'}, {var, 0, 'Buffer'}, {var, 0, 'ClientHandler'}], [], Body},
    {function, 0, ?SUCCESS_FIN_NAME, 3, [FunClause]}.

-spec generate_fail_finalizator() -> tuple().
generate_fail_finalizator() ->
    %% fail_finalizator(ReturnValue, Buffer, ClientHandler) -> ok
    SetupGuard = generate_trap_guard(true),
    SendOutput = generate_send_output(),
    FinishCommand = generate_finish_command({var, 0, 'ReturnValue'}),
    ReleaseGuard = generate_trap_guard(false),
    Body = [SetupGuard, SendOutput, FinishCommand, ReleaseGuard],
    FunClause = {clause, 0, [{var, 0, 'ReturnValue'}, {var, 0, 'Buffer'}, {var, 0, 'ClientHandler'}], [], Body},
    {function, 0, ?FAIL_FIN_NAME, 3, [FunClause]}.

-spec generate_trap_guard(TrapExit :: boolean()) -> tuple().
generate_trap_guard(TrapExit) ->
    GuardArgs = [{atom, 0, trap_exit}, {atom, 0, TrapExit}],
    {call, 0, {remote, 0, {atom, 0, erlang}, {atom, 0, process_flag}}, GuardArgs}.

-spec generate_fsm_notification(CommandName :: atom()) -> tuple().
generate_fsm_notification(CommandName) ->
    NotificationArgs = [{atom, 0, CommandName}, {var, 0, 'CliFsm'}],
    {call, 0, {remote, 0, {atom, 0, cli_fsm}, {atom, 0, process_command}}, NotificationArgs}.

-spec generate_send_output() -> tuple().
generate_send_output() ->
    DataRetrieveArgs = [{var, 0, 'Buffer'}, {atom, 0, both}],
    DataRetrieve = {call, 0, {remote, 0, {atom, 0, io_buffer}, {atom, 0, get_data}}, DataRetrieveArgs},
    DataDef = {match, 0, {var, 0, 'OutputData'}, DataRetrieve},
    SendOutputPattern = [{tuple, 0, [{atom, 0, output}, {var, 0, 'Message'}]}],
    SendOutputCallArgs = [{var, 0, 'ClientHandler'}, {var, 0, 'Message'}],
    SendOutputCall = {call, 0, {remote, 0, {atom, 0, client_handler}, {atom, 0, send_output}}, SendOutputCallArgs},
    SendOutputClause = {clause, 0, SendOutputPattern, [], [SendOutputCall]},
    SendErrorPattern = [{tuple, 0, [{atom, 0, error}, {var, 0, 'Message'}]}],
    SendErrorCallArgs = [{var, 0, 'ClientHandler'}, {var, 0, 'Message'}],
    SendErrorCall = {call, 0, {remote, 0, {atom, 0, client_handler}, {atom, 0, send_error}}, SendErrorCallArgs},
    SendErrorClause = {clause, 0, SendErrorPattern, [], [SendErrorCall]},
    ForeachFun = {'fun', 0, {clauses, [SendOutputClause, SendErrorClause]}},
    ForeachArgs = [ForeachFun, {var, 0, 'OutputData'}],
    Foreach = {call, 0, {remote, 0, {atom, 0, lists}, {atom, 0, foreach}}, ForeachArgs},
    [DataDef, Foreach].

-spec generate_finish_command(ReturnValue :: tuple()) -> tuple().
generate_finish_command(ReturnValue) ->
    FinishCommandArgs = [{var, 0, 'ClientHandler'}, ReturnValue],
    {call, 0, {remote, 0, {atom, 0, client_handler}, {atom, 0, finish_command}}, FinishCommandArgs}.