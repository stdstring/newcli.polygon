%% @author stdstring

-module(cli_frontend_application).

-include("message_defs.hrl").
-include("common_defs.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

-export([main/0, main/1]).

main() -> main("/tmp/frontend.conf").

main(MainConfigFile) ->
    GlobalConfig = config_reader:read(MainConfigFile),
    ExecutionState = init_execution_state(GlobalConfig),
    main_worker(GlobalConfig, ExecutionState).

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec init_execution_state(GlobalConfig :: #global_config{}) -> #execution_state{}.
init_execution_state(GlobalConfig) ->
    GlobalHandler = GlobalConfig#global_config.global_handler,
    CommandsInfo = retrieve_commands_info(GlobalHandler),
    #execution_state{global_handler = GlobalHandler, commands_info = CommandsInfo}.

-spec retrieve_commands_info(GlobalHandler :: term()) -> [{CommandName :: atom(), CommandBody :: [string()], CommandHelp :: string()}] | no_return().
retrieve_commands_info(GlobalHandler) ->
    case gen_server:call(GlobalHandler, #commands_info{}) of
        #commands_info_result{info = CommandsInfo} ->
            lists:map(fun(#command_info{command_name = Name, command_body = Body, command_help = Help}) -> {Name, Body, Help} end, CommandsInfo);
        #commands_info_fail{reason = Reason} ->
            io:fotmat(standard_info, "Command's info retrieving is failed due to the following: ~p~n", [Reason]),
            error({commands_info, Reason})
    end.

-spec generate_prompt(ExecutionState :: #execution_state{}) -> string().
generate_prompt(ExecutionState) ->
    LoginInfo = ExecutionState#execution_state.login_info,
    CliMode = ExecutionState#execution_state.current_cli_mode,
    DeviceName = "CliDemo",
    "frontend_test>".

main_worker(GlobalConfig, ExecutionState) ->
    Prompt = generate_prompt(ExecutionState),
    CommandLine = io:get_line(Prompt),
    Command = string_data_utils:remove_trailing_line_feed(CommandLine),
    NewExecutionState = command_execution_context:execute(Command, GlobalConfig, ExecutionState),
    main_worker(GlobalConfig, NewExecutionState).