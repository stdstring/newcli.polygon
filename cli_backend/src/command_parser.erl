%% @author std-string

-module(command_parser).

-include("common_defs.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

-export([parse/3]).

-spec parse(CommandLine :: string(), GlobalConfig :: #global_config{}, OutputEndpoint :: pid()) ->
          [{ModuleName :: atom, CommandPid :: pid}] | {'command_parser', Reason :: term()}.
parse(CommandLine, GlobalConfig, OutputEndpoint) when is_record(GlobalConfig, global_config) ->
    Commands = GlobalConfig#global_config.commands,
    case find_command(CommandLine, Commands) of
        {_, CommandModule, CommandLineRest} -> create_command_chain(CommandModule, CommandLineRest, OutputEndpoint);
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
            find_command(CommandParserFsm, string:strip(CommandLine), #parse_result{state = ambiguous_parsing, can_continue = true}, undefined)
    end.

-spec find_command(CommandParserFsm :: pid(),
                   Rest :: string(),
                   PrevResult :: #parse_result{},
                   RecognizedCommand :: {CommandName :: atom(), CommandModule :: atom(), CommandLineRest :: string()} | 'undefined') ->
          {CommandName :: atom(), CommandModule :: atom(), ComandLineRest :: string()} | {'false', Reason :: term()}.
%% when Rest == ""
find_command(_CommandParserFsm, "", #parse_result{state = incomplete_parsing}, undefined) -> {false, incomplete_command};
find_command(_CommandParserFsm, "", #parse_result{state = ambiguous_parsing}, undefined) -> {false, ambiguous_command};
find_command(_CommandParserFsm, "", #parse_result{state = successful_parsing, command = {Name, Module}}, _) -> {Name, Module, ""};
find_command(_CommandParserFsm, "", _Result, RecognizedCommand) -> RecognizedCommand;
%% when Rest /= "" and PrevResult == #parse_result{state = successful_parsing}
find_command(CommandParserFsm, Rest, #parse_result{state = successful_parsing, command = {Name, Module}, can_continue = true}, _) ->
    RecognizedCommand = {Name, Module, Rest},
    process_token_parse(CommandParserFsm, Rest, RecognizedCommand);
find_command(_CommandParserFsm, Rest, #parse_result{state = successful_parsing, command = {Name, Module}, can_continue = false}, _) ->
    {Name, Module, Rest};
%% when Rest /= "" and PrevResult == #parse_result{state = unsuccessful_parsing}
find_command(_CommandParserFsm, _Rest, #parse_result{state = unsuccessful_parsing}, undefined) -> {false, unknown_command};
find_command(_CommandParserFsm, _Rest, #parse_result{state = unsuccessful_parsing}, RecognizedCommand) -> RecognizedCommand;
%% common case
find_command(CommandParserFsm, Rest, _Result, RecognizedCommand) ->
    process_token_parse(CommandParserFsm, Rest, RecognizedCommand).

-spec process_token_parse(CommandParserFsm :: pid(),
                          Rest :: string(),
                          RecognizedCommand :: {CommandName :: atom(), CommandModule :: atom(), CommandLineRest :: string()} | 'undefined') ->
          no_return().
process_token_parse(CommandParserFsm, Rest, RecognizedCommand) ->
    {Token, NewRest} = commandline_parser:get_first_token(Rest),
    Result = command_parser_fsm:process_token(CommandParserFsm, Token),
    find_command(CommandParserFsm, NewRest, Result, RecognizedCommand).

-spec create_command_chain(CommandModule :: atom(), CommandLineRest :: string(), OutputEndpoint :: pid()) -> [{ModuleName :: atom, CommandPid :: pid}].
create_command_chain(CommandModule, CommandLineRest, OutputEndpoint) ->
    Stdout = OutputEndpoint,
    Stderr = OutputEndpoint,
    case apply(CommandModule, create, [CommandLineRest, Stdout, Stderr]) of
        {error, Reason} ->
            CommandName = apply(CommandModule, get_name, []),
            {command_parser, {CommandName, creation_error, Reason}};
        CommandPid -> [{CommandModule, CommandPid}]
    end.
