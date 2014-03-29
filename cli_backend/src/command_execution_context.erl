%% @author std-string

-module(command_execution_context).

-include("message_defs.hrl").
-include("common_defs.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

-export([execute/3]).

-spec execute(CommandLine :: string(), Config :: #config{}, ClientState :: #client_state{}) -> boolean().
execute(CommandLine, Config, ClientState) ->
    User = ClientState#client_state.user,
    CliFsm = ClientState#client_state.cli_fsm,
    ClientOutput = ClientState#client_state.output,
    case command_parser:parse(CommandLine, Config, ClientOutput) of
        {command_parser, Reason} ->
            send_error(ClientOutput, {command_parser, Reason}, CommandLine, CliFsm),
            StateInfo = cli_fsm:get_current_state(CliFsm),
            not StateInfo#cli_fsm_state_info.is_terminal;
        Commands -> execute(Commands, ClientOutput, CliFsm, User)
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec execute(Commands :: [{CommandModule :: atom(), CommandPid :: pid()}], Endpoint :: pid(), CliFsm :: pid(), User :: #user{}) -> boolean().
execute([], Endpoint, CliFsm, _User) ->
    #cli_fsm_state_info{current_state = CurrentState, is_terminal = IsTerminalState} = cli_fsm:get_current_state(CliFsm),
    output_endpoint:send_to_client(Endpoint, 0, CurrentState),
    not IsTerminalState;
execute([{CommandModule, CommandPid} | Commands], Endpoint, CliFsm, User) ->
    CommandName = apply(CommandModule, get_name, []),
    case command_execution_checker:execution_precheck(CommandName, CliFsm, User) of
        {false, Reason} ->
            send_error(Endpoint, Reason, CommandName, CliFsm),
            StateInfo = cli_fsm:get_current_state(CliFsm),
            not StateInfo#cli_fsm_state_info.is_terminal;
        true -> process_execute(CommandModule, CommandPid, Commands, Endpoint, CliFsm, User)
    end.

-spec process_execute(CommandModule :: atom(),
                      CommandPid :: pid(),
                      OtherCommands :: [{CommandModule :: atom(), CommandPid :: pid()}],
                      Endpoint :: pid(),
                      CliFsm :: pid(),
                      User :: #user{}) -> boolean().
process_execute(CommandModule, CommandPid, OtherCommands, Endpoint, CliFsm, User) ->
    CommandName = apply(CommandModule, get_name, []),
    ReturnCode = apply(CommandModule, execute, [CommandPid]),
    if
        ReturnCode == 0 ->
            cli_fsm:process_command(CliFsm, CommandName),
            execute(OtherCommands, Endpoint, CliFsm, User);
        ReturnCode /= 0 ->
            send_error(Endpoint, ReturnCode, CliFsm),
            StateInfo = cli_fsm:get_current_state(CliFsm),
            not StateInfo#cli_fsm_state_info.is_terminal
    end.

-spec send_error(Endpoint :: pid(), ReturnCode :: integer(), CliFsm :: pid()) -> 'ok'.
send_error(Endpoint, ReturnCode, CliFsm) ->
    #cli_fsm_state_info{current_state = CurrentState} = cli_fsm:get_current_state(CliFsm),
    output_endpoint:send_to_client(Endpoint, ReturnCode, CurrentState),
    ok.

-spec send_error(Endpoint :: pid(), Reason :: term(), CommandName :: atom(), CliFsm :: pid()) -> 'ok'.
send_error(Endpoint, {command_parser, Reason}, CommandLine, CliFsm) ->
    Message = lists:flatten(io_lib:format("Parsing of command line \"~s\" is failed due to the following reason: \"~s\"", [CommandLine, Reason])),
    send_error_message(Endpoint, Message, CliFsm),
    ok;
send_error(Endpoint, Reason, CommandName, CliFsm) ->
    Message = lists:flatten(io_lib:format("Command \"~s\" is failed due to the following reason: \"~s\"", [CommandName, Reason])),
    send_error_message(Endpoint, Message, CliFsm),
    ok.

-spec send_error_message(Endpoint :: pid(), Message :: string(), CliFsm :: pid()) -> ok.
send_error_message(Endpoint, Message, CliFsm) ->
    gen_server:cast(Endpoint, #command_error{message = Message}),
    #cli_fsm_state_info{current_state = CurrentState} = cli_fsm:get_current_state(CliFsm),
    output_endpoint:send_to_client(Endpoint, 255, CurrentState),
    ok.
