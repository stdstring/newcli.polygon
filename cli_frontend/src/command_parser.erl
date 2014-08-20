%% @author std-string

-module(command_parser).

-include("common_defs.hrl").

-define(HELP, "?").

%% ====================================================================
%% API functions
%% ====================================================================

-export([parse/2]).

-spec parse(CommandLine :: string(), GlobalConfig :: #global_config{}) -> [#command_entry{}] | {'command_parser', Reason :: term()}.
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
            Result = find_command_impl(CommandParserFsm, string:strip(CommandLine)),
            command_parser_fsm:stop(CommandParserFsm),
            Result
    end.

-spec find_command_impl(CommandParserFsm :: pid(),
                        CommandLine :: string()) ->{CommandName :: atom(), CommandModule :: atom(), ComandLineRest :: string()} | {'false', Reason :: term()}.
find_command_impl(CommandParserFsm, CommandLine) ->
    %% special case for help command
    IsHelpCommand = lists:suffix(?HELP, CommandLine),
    if
        IsHelpCommand == true ->
            HelpCommandRest = lists:sublist(CommandLine, length(CommandLine) - length(?HELP)),
            {help_command, help_command, HelpCommandRest};
        IsHelpCommand == false ->
            find_command_impl(CommandParserFsm, CommandLine, CommandLine, #ambiguous_parse_result{}, undefined)
    end.

-spec find_command_impl(CommandParserFsm :: pid(),
                        CommandLine :: string(),
                        Rest :: string(),
                        PrevResult :: #ambiguous_parse_result{} | #incomplete_parse_result{} | #unsuccessful_parse_result{} | #successful_parse_result{},
                        RecognizedCommand :: {CommandName :: atom(), CommandModule :: atom(), CommandLineRest :: string()} | 'undefined') ->
          {CommandName :: atom(), CommandModule :: atom(), ComandLineRest :: string()} | {'false', Reason :: term()}.
%% when Rest == ""
find_command_impl(_CommandParserFsm, _CommandLine, "", #successful_parse_result{command = {Name, Module}}, _) ->
    {Name, Module, ""};
find_command_impl(_CommandParserFsm, CommandLine, "", _Result, undefined) -> {backend_command, backend_command, CommandLine};
find_command_impl(_CommandParserFsm, _CommandLine, "", _Result, RecognizedCommand) -> RecognizedCommand;
%% when Rest /= "" and PrevResult == #successful_parse_result{}
find_command_impl(CommandParserFsm, CommandLine, Rest, #successful_parse_result{command = {Name, Module}, can_continue = true}, _) ->
    RecognizedCommand = {Name, Module, Rest},
    process_token_parse(CommandParserFsm, CommandLine, Rest, RecognizedCommand);
find_command_impl(_CommandParserFsm, _CommandLine, Rest, #successful_parse_result{command = {Name, Module}, can_continue = false}, _) ->
    {Name, Module, Rest};
%% when Rest /= "" and PrevResult == #unsuccessful_parse_result{}
find_command_impl(_CommandParserFsm, CommandLine, _Rest, #unsuccessful_parse_result{}, undefined) ->
    {backend_command, backend_command, CommandLine};
find_command_impl(_CommandParserFsm, _CommandLine, _Rest, #unsuccessful_parse_result{}, RecognizedCommand) -> RecognizedCommand;
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

-spec create_command_chain(CommandModule :: atom(), CommandLineRest :: string()) -> [#command_entry{}].
create_command_chain(CommandModule, CommandLineRest) ->
    [#command_entry{module = CommandModule, command_line_rest = CommandLineRest}].
