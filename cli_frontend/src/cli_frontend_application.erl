%% @author stdstring

-module(cli_frontend_application).

-include("logic_utils_defs.hrl").
-include("message_defs.hrl").
-include("common_defs.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

-export([main/0, main/1]).

main() -> main("/tmp/frontend/frontend.conf").

main(MainConfigFile) ->
    GlobalConfig = config_reader:read(MainConfigFile),
    ExecutionState = init_execution_state(GlobalConfig),
    ConsoleOpts = create_console_opts(ExecutionState),
    main_worker(GlobalConfig, ExecutionState, ConsoleOpts).

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
            io:fotmat(standard_error, "Command's info retrieving is failed due to the following: ~w~n", [Reason]),
            error({commands_info, Reason})
    end.

-spec create_console_opts(ExecutionState :: #execution_state{}) -> [{Key :: atom(), Option :: term()}].
create_console_opts(ExecutionState) ->
    ExpandFun = autocomplete_factory:create_expand_fun(ExecutionState),
    [{expand_fun, ExpandFun}, {echo, true}, {binary, false}, {encoding, unicode}].

-spec generate_prompt(ExecutionState :: #execution_state{}) -> string().
generate_prompt(ExecutionState) ->
    LoginInfo = ExecutionState#execution_state.login_info,
    LoginPart = logic_utils:ternary_lazy_op(LoginInfo == undefined, ?LAZY(""), ?LAZY(LoginInfo#login_info.login_name)),
    DeviceName = "CliDemo",
    CliMode = ExecutionState#execution_state.current_cli_mode,
    CliModePart = logic_utils:ternary_lazy_op((CliMode == undefined) or (CliMode == ""), ?LAZY(""), ?LAZY(" (" ++ CliMode ++ ")")),
    IsAdmin = logic_utils:ternary_lazy_op(LoginInfo == undefined, ?LAZY(false), ?LAZY(LoginInfo#login_info.is_admin)),
    PromptFooter = logic_utils:ternary_op(IsAdmin, "#", ">"),
    Prompt = io_lib:format("~s@~s~s~s", [LoginPart, DeviceName, CliModePart, PromptFooter]),
    lists:flatten(Prompt).

-spec main_worker(GlobalConfig :: #global_config{},
                  ExecutionState :: #execution_state{},
                  ConsoleOpts :: [{Key :: atom(), Option :: term()}]) -> no_return().
main_worker(GlobalConfig, ExecutionState, ConsoleOpts) ->
    io:setopts(ConsoleOpts),
    Prompt = generate_prompt(ExecutionState),
    case io:get_line(Prompt) of
        {error, ErrorDescription} -> {error, ErrorDescription};
        eof -> eof;
        CommandLine ->
            Command = string_data_utils:remove_trailing_line_feed(CommandLine),
            NewExecutionState = command_execution_context:execute(Command, GlobalConfig, ExecutionState),
            main_worker(GlobalConfig, NewExecutionState, ConsoleOpts)
    end.