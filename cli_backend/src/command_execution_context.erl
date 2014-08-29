%% @author std-string

-module(command_execution_context).

-include("message_defs.hrl").
-include("common_defs.hrl").

-record(parser_fail, {command_line = "" :: string(), reason = undefined :: 'undefined' | term()}).
-record(precondition_check_fail, {reason = undefined :: 'undefined' | term()}).

%% ====================================================================
%% API functions
%% ====================================================================

-export([execute/4]).

-spec execute(CommandLine :: string(), GlobalConfig :: #global_config{}, ClientConfig :: #client_config{}, ClientOutput :: pid()) -> boolean().
execute(CommandLine, GlobalConfig, ClientConfig, ClientOutput) ->
    io:format("ClientConfig = ~p~n", [ClientConfig]),
    User = ClientConfig#client_config.user,
    CliFsm = ClientConfig#client_config.cli_fsm,
    Endpoint = create_output_endpoint(ClientOutput),
    case command_parser:parse(CommandLine, GlobalConfig, Endpoint) of
        {command_parser, Reason} ->
            send_fail(Endpoint, #parser_fail{command_line = CommandLine, reason = Reason}, CliFsm),
            StateInfo = cli_fsm:get_current_state(CliFsm),
            not StateInfo#cli_fsm_state_info.is_terminal;
        Commands -> execute_impl(Commands, Endpoint, CliFsm, User)
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec create_output_endpoint(ClientOutput :: pid()) -> pid() | no_return().
create_output_endpoint(ClientOutput) ->
    case output_endpoint:start(ClientOutput) of
        {ok, Pid} -> Pid;
        {error, Reason} -> error({output_endpoint, Reason})
    end.

-spec execute_impl(Commands :: [{CommandModule :: atom(), CommandPid :: pid()}], Endpoint :: pid(), CliFsm :: pid(), User :: #user{}) -> boolean().
execute_impl([], Endpoint, CliFsm, _User) ->
    #cli_fsm_state_info{current_state_representation = Representation, is_terminal = IsTerminalState} = cli_fsm:get_current_state(CliFsm),
    output_endpoint:send_result(Endpoint, 0, Representation),
    not IsTerminalState;
execute_impl([{CommandModule, CommandPid} | Commands], Endpoint, CliFsm, User) ->
    CommandName = apply(CommandModule, get_name, []),
    case command_execution_checker:execution_precheck(CommandName, CliFsm, User) of
        {false, Reason} ->
            send_fail(Endpoint, #precondition_check_fail{reason = Reason}, CliFsm),
            StateInfo = cli_fsm:get_current_state(CliFsm),
            not StateInfo#cli_fsm_state_info.is_terminal;
        true -> process_execute(CommandModule, CommandPid, Commands, Endpoint, CliFsm, User)
    end.

-spec process_execute(CommandModule :: atom(),
                      CommandPid :: pid(),
                      OtherCommands :: [{CommandModule :: atom(), CommandPid :: pid()}],
                      Endpoint :: pid(),
                      CliFsm :: pid(),
                      User :: #user{}) -> no_return() | boolean().
process_execute(CommandModule, CommandPid, OtherCommands, Endpoint, CliFsm, User) ->
    CommandName = apply(CommandModule, get_name, []),
    ReturnCode = apply(CommandModule, execute, [CommandPid]),
    if
        ReturnCode == 0 ->
            cli_fsm:process_command(CliFsm, CommandName),
            execute_impl(OtherCommands, Endpoint, CliFsm, User);
        ReturnCode /= 0 ->
            send_error(Endpoint, ReturnCode, CliFsm),
            StateInfo = cli_fsm:get_current_state(CliFsm),
            not StateInfo#cli_fsm_state_info.is_terminal
    end.

-spec send_error(Endpoint :: pid(), ReturnCode :: integer(), CliFsm :: pid()) -> 'ok'.
send_error(Endpoint, ReturnCode, CliFsm) ->
    #cli_fsm_state_info{current_state_representation = Representation} = cli_fsm:get_current_state(CliFsm),
    output_endpoint:send_result(Endpoint, ReturnCode, Representation),
    ok.

-spec send_fail(Endpoint :: pid(), Message :: term(), CliFsm :: pid()) -> 'ok'.
send_fail(Endpoint, Message, CliFsm) ->
    #cli_fsm_state_info{current_state_representation = Representation} = cli_fsm:get_current_state(CliFsm),
    output_endpoint:send_fail(Endpoint, Message, Representation),
    ok.

