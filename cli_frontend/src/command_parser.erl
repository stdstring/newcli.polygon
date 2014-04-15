%% @author std-string

-module(command_parser).

-include("common_defs.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

-export([parse/2]).

-spec parse(CommandLine :: string(), GlobalConfig :: #global_config{}) ->
          [{ModuleName :: atom, CommandPid :: pid}] | {'command_parser', Reason :: term()}.
parse(CommandLine, GlobalConfig) when is_record(GlobalConfig, global_config) ->
    Commands = GlobalConfig#global_config.commands,
    case find_command(CommandLine, Commands) of
        {_, CommandModule, CommandLineRest} -> create_command_chain(CommandModule, CommandLineRest);
        {false, Reason} -> {command_parser, Reason}
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec find_command(CommandLine :: string(), Commands :: [{CommandName :: atom(), CommandModule :: atom()}]) ->
          {CommandName :: atom(), CommandModule :: atom(), ComandLineRest :: string()} | {'false', Reason :: term()}.
find_command(CommandLine, Commands) ->
    case command_parser_fsm:start(Commands) of
        {command_parser_fsm, Error} ->
            {false, {command_parser_fsm, Error}};
        {ok, CommandParserFsm} ->
            find_command_impl(CommandParserFsm, string:strip(CommandLine))
    end.

-spec find_command_impl(CommandParserFsm :: pid(),
                        CommandLine :: string()) ->{CommandName :: atom(), CommandModule :: atom(), ComandLineRest :: string()} | {'false', Reason :: term()}.
find_command_impl(CommandParserFsm, CommandLine) ->
    %% special case for help command
    IsHelpCommand = lists:suffix("?", CommandLine),
    if
        IsHelpCommand == true ->
            HelpCommandRest = lists:sublist(CommandLine, length(CommandLine) - length("?")),
            {help_command, help_command, HelpCommandRest};
        IsHelpCommand == false ->
            find_command_impl(CommandParserFsm, CommandLine, CommandLine, #parse_result{state = ambiguous_parsing, can_continue = true}, undefined)
    end.

-spec find_command_impl(CommandParserFsm :: pid(),
                        CommandLine :: string(),
                        Rest :: string(),
                        PrevResult :: #parse_result{},
                        RecognizedCommand :: {CommandName :: atom(), CommandModule :: atom(), CommandLineRest :: string()} | 'undefined') ->
          {CommandName :: atom(), CommandModule :: atom(), ComandLineRest :: string()} | {'false', Reason :: term()}.
%% when Rest == ""
find_command_impl(_CommandParserFsm, _CommandLine, "", #parse_result{state = successful_parsing, command = {Name, Module}}, _) ->
    {Name, Module, ""};
find_command_impl(_CommandParserFsm, CommandLine, "", _Result, undefined) -> {backend_command, backend_command, CommandLine};
find_command_impl(_CommandParserFsm, _CommandLine, "", _Result, RecognizedCommand) -> RecognizedCommand;
%% when Rest /= "" and PrevResult == #parse_result{state = successful_parsing}
find_command_impl(CommandParserFsm, CommandLine, Rest, #parse_result{state = successful_parsing, command = {Name, Module}, can_continue = true}, _) ->
    RecognizedCommand = {Name, Module, Rest},
    process_token_parse(CommandParserFsm, CommandLine, Rest, RecognizedCommand);
find_command_impl(_CommandParserFsm, _CommandLine, Rest, #parse_result{state = successful_parsing, command = {Name, Module}, can_continue = false}, _) ->
    {Name, Module, Rest};
%% when Rest /= "" and PrevResult == #parse_result{state = unsuccessful_parsing}
find_command_impl(_CommandParserFsm, CommandLine, _Rest, #parse_result{state = unsuccessful_parsing}, undefined) ->
    {backend_command, backend_command, CommandLine};
find_command_impl(_CommandParserFsm, _CommandLine, _Rest, #parse_result{state = unsuccessful_parsing}, RecognizedCommand) -> RecognizedCommand;
%% common case
find_command_impl(CommandParserFsm, CommandLine, Rest, _Result, RecognizedCommand) ->
    process_token_parse(CommandParserFsm, CommandLine, Rest, RecognizedCommand).

-spec process_token_parse(CommandParserFsm :: pid(),
                          CommandLine :: string(),
                          Rest :: string(),
                          RecognizedCommand :: {CommandName :: atom(), CommandModule :: atom(), CommandLineRest :: string()} | 'undefined') ->
          no_return().
process_token_parse(CommandParserFsm, CommandLine, Rest, RecognizedCommand) ->
    {Token, NewRest} = commandline_parser:get_first_token(Rest),
    Result = command_parser_fsm:process_token(CommandParserFsm, Token),
    find_command_impl(CommandParserFsm, CommandLine, NewRest, Result, RecognizedCommand).

-spec create_command_chain(CommandModule :: atom(), CommandLineRest :: string()) -> [{ModuleName :: atom, CommandPid :: pid}].
create_command_chain(CommandModule, CommandLineRest) ->
    case apply(CommandModule, create, [CommandLineRest]) of
        {error, Reason} ->
            CommandName = apply(CommandModule, get_name, []),
            {command_parser, {CommandName, creation_error, Reason}};
        {ok, CommandPid} -> [{CommandModule, CommandPid}]
    end.
