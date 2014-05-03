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
            Result = find_command(CommandParserFsm, string:strip(CommandLine), #ambiguous_parse_result{}, undefined),
            command_parser_fsm:stop(CommandParserFsm),
            Result
    end.

-spec find_command(CommandParserFsm :: pid(),
                   Rest :: string(),
                   PrevResult :: #ambiguous_parse_result{} | #incomplete_parse_result{} | #unsuccessful_parse_result{} | #successful_parse_result{},
                   RecognizedCommand :: {CommandName :: atom(), CommandModule :: atom(), CommandLineRest :: string()} | 'undefined') ->
          {CommandName :: atom(), CommandModule :: atom(), ComandLineRest :: string()} | {'false', Reason :: term()}.
%% when Rest == ""
find_command(_CommandParserFsm, "", #unsuccessful_parse_result{}, undefined) -> {false, unknown_command};
find_command(_CommandParserFsm, "", #incomplete_parse_result{}, undefined) -> {false, incomplete_command};
find_command(_CommandParserFsm, "", #ambiguous_parse_result{}, undefined) -> {false, ambiguous_command};
find_command(_CommandParserFsm, "", #successful_parse_result{command = {Name, Module}}, _) -> {Name, Module, ""};
find_command(_CommandParserFsm, "", _Result, RecognizedCommand) -> RecognizedCommand;
%% when Rest /= "" and PrevResult == #successful_parse_result{}
find_command(CommandParserFsm, Rest, #successful_parse_result{command = {Name, Module}, can_continue = true}, _) ->
    RecognizedCommand = {Name, Module, Rest},
    process_token_parse(CommandParserFsm, Rest, RecognizedCommand);
find_command(_CommandParserFsm, Rest, #successful_parse_result{command = {Name, Module}, can_continue = false}, _) ->
    {Name, Module, Rest};
%% when Rest /= "" and PrevResult == #unsuccessful_parse_result{}
find_command(_CommandParserFsm, _Rest, #unsuccessful_parse_result{}, undefined) -> {false, unknown_command};
find_command(_CommandParserFsm, _Rest, #unsuccessful_parse_result{}, RecognizedCommand) -> RecognizedCommand;
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
        {ok, CommandPid} -> [{CommandModule, CommandPid}]
    end.
