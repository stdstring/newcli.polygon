%% @author std-string

-module(client_handler_helper).

-include("authentication_defs.hrl").
-include("common_defs.hrl").
-include("name_search_defs.hrl").

-export([get_help/2, get_suitable_commands/2]).

%% ====================================================================
%% API functions
%% ====================================================================

-spec get_help(CommandLine :: string(), State :: #client_handler_state{}) -> string().
get_help(CommandLine, State) ->
    GlobalConfig = State#client_handler_state.config,
    %% TODO (std_string) : think about caching
    NameConfig = name_search_config:create(GlobalConfig#global_config.commands),
    CliFsm = State#client_handler_state.cli_fsm,
    User = State#client_handler_state.user,
    SuitableConfig = filter_name_search(NameConfig, CliFsm, User),
    Words = commandline_parser:parse(CommandLine),
    case name_search_helper:search_exact(Words, SuitableConfig) of
        {true, CommandName} -> get_command_help(CommandName, GlobalConfig);
        false -> ""
    end.

-spec get_suitable_commands(CommandLine :: string(), State :: #client_handler_state{}) ->
    {CommonPrefix :: string(), Commands :: [string()]}.
get_suitable_commands(CommandLine, State) ->
    GlobalConfig = State#client_handler_state.config,
    Words = get_words(CommandLine),
    {CommonPrefix, SuitableCommands} = get_commands(Words, State),
    {CommonPrefix, lists:map(fun(Name) -> string:join(get_command_body(Name, GlobalConfig), " ") end, SuitableCommands)}.

%% ====================================================================
%% Internal functions
%% ====================================================================

get_words("") -> [];
get_words(CommandLine) ->
    case lists:last(CommandLine) of
        $\s -> commandline_parser:parse(CommandLine) ++ [""];
        _Other -> commandline_parser:parse(CommandLine)
    end.

-spec get_commands(Words :: [string()], State :: #client_handler_state{}) ->
    {CommonPrefix :: string(), SuitableCommands :: [atom()]}.
get_commands(Words, State) ->
    GlobalConfig = State#client_handler_state.config,
    %% TODO (std_string) : think about caching
    NameConfig = name_search_config:create(GlobalConfig#global_config.commands),
    CliFsm = State#client_handler_state.cli_fsm,
    User = State#client_handler_state.user,
    SuitableConfig = filter_name_search(NameConfig, CliFsm, User),
    name_search_helper:search_suitable(Words, SuitableConfig).

-spec filter_name_search(NameConfig :: name_search_table(), CliFsm :: pid(), User :: #user{} | 'undefined') ->
    name_search_table().
filter_name_search(NameConfig, CliFsm, User) ->
    SuitableCommands = command_execution_checker:select_suitable_commands(CliFsm, User),
    FilterFun = fun({_SearchItems, CommandName}) ->
        lists:any(fun(Name) -> Name == CommandName end, SuitableCommands)
    end,
    lists:filter(FilterFun, NameConfig).

-spec get_command_help(CommandName :: atom(), GlobalConfig :: #global_config{}) -> [string()].
get_command_help(CommandName, GlobalConfig) ->
    CommandModule = command_search:search_by_name(CommandName, GlobalConfig),
    CommandModule:get_help().

-spec get_command_body(CommandName :: atom(), GlobalConfig :: #global_config{}) -> [string()].
get_command_body(CommandName, GlobalConfig) ->
    CommandModule = command_search:search_by_name(CommandName, GlobalConfig),
    CommandModule:get_command_body().
