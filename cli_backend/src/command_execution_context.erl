%% @author std-string

-module(command_execution_context).

-include("message_defs.hrl").
-include("common_defs.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

-export([execute/3]).

-spec execute(CommandLine :: string(), GlobalConfig :: #global_config{}, ClientConfig :: #client_config{}) -> boolean().
execute(CommandLine, GlobalConfig, ClientConfig) ->
    User = ClientConfig#client_config.user,
    CliFsm = ClientConfig#client_config.cli_fsm,
    ClientOutput = ClientConfig#client_config.output,
    case command_parser:parse(CommandLine, GlobalConfig, ClientOutput) of
        {command_parser, Reason} ->
            send_parser_error(ClientOutput, Reason, CommandLine, CliFsm),
            StateInfo = cli_fsm:get_current_state(CliFsm),
            not StateInfo#cli_fsm_state_info.is_terminal;
        #command_parse_result{command_chain = Commands, endpoint = Endpoint} ->
            execute(Commands, Endpoint, CliFsm, User)
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
send_error(Endpoint, Reason, CommandName, CliFsm) ->
    Message = lists:flatten(io_lib:format("Command \"~p\" is failed due to the following reason: \"~p\"", [CommandName, Reason])),
    send_error_message(Endpoint, Message, CliFsm),
    ok.

-spec send_parser_error(Endpoint :: pid(),  Reason :: term(), CommandLine :: string(), CliFsm :: pid()) -> 'ok'.
send_parser_error(Endpoint, Reason, CommandLine, CliFsm) ->
    Message = lists:flatten(io_lib:format("Parsing of command line \"~s\" is failed due to the following reason: \"~p\"", [CommandLine, Reason])),
    send_error_message(Endpoint, Message, CliFsm),
    ok.

-spec send_error_message(Endpoint :: pid(), Message :: string(), CliFsm :: pid()) -> ok.
send_error_message(Endpoint, Message, CliFsm) ->
    gen_server:cast(Endpoint, #command_error{message = Message}),
    #cli_fsm_state_info{current_state = CurrentState} = cli_fsm:get_current_state(CliFsm),
    output_endpoint:send_to_client(Endpoint, 255, CurrentState),
    ok.
