%% @author std-string

-module(client_handler_helper).

-include("authentication_defs.hrl").
-include("common_defs.hrl").

-export([get_help/2, get_suitable_commands/2]).

%% ====================================================================
%% API functions
%% ====================================================================

-spec get_help(CommandLine :: string(), State :: #client_handler_state{}) -> string().
get_help(CommandLine, State) ->
    GlobalConfig = State#client_handler_state.config,
    %% TODO (std_string) : think about caching
    NameConfig = name_search_config:create(GlobalConfig#global_config.commands),
    Words = commandline_parser:parse(CommandLine),
    case name_search_helper:search_exact(Words, NameConfig) of
        {true, CommandModule} ->
            CommandName = CommandModule:get_name(),
            CliFsm = State#client_handler_state.cli_fsm,
            User = State#client_handler_state.user,
            case command_execution_checker:execution_precheck(CommandName, CliFsm, User) of
                true -> CommandModule:get_help();
                {false, _Reason} -> ""
            end;
        false -> ""
    end.

-spec get_suitable_commands(CommandLine :: string(), State :: #client_handler_state{}) ->
    {CommonPrefix :: string(), Commands :: [string()]}.
get_suitable_commands(CommandLine, State) ->
    Words = commandline_parser:parse(CommandLine),
    {CommonPrefix, SuitableCommands} = get_commands(Words, State),
    {CommonPrefix, lists:map(fun(Command) -> string:join(Command:get_command_body(), " ") end, SuitableCommands)}.

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec get_commands(Words :: [string()], State :: #client_handler_state{}) ->
    {CommonPrefix :: string(), SuitableCommands :: [atom()]}.
get_commands(Words, State) ->
    GlobalConfig = State#client_handler_state.config,
    %% TODO (std_string) : think about caching
    NameConfig = name_search_config:create(GlobalConfig#global_config.commands),
    {CommonPrefix, Commands} = name_search_helper:search_suitable(Words, NameConfig),
    CliFsm = State#client_handler_state.cli_fsm,
    User = State#client_handler_state.user,
    {CommonPrefix, command_execution_checker:select_suitable_commands(Commands, CliFsm, User)}.