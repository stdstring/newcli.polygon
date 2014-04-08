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
        {command_parser_fsm, Error} -> {false, {command_parser_fsm, Error}};
        CommandParserFsm -> find_command(CommandParserFsm, string:strip(CommandLine), undefined)
    end.

-spec find_command(CommandParserFsm :: pid(), Rest :: string(), Result :: term()) ->
          {CommandName :: atom(), CommandModule :: atom(), ComandLineRest :: string()} | {'false', Reason :: term()}.
find_command(_CommandParserFsm, Rest, {successful_parsing, {Name, Module}}) -> {Name, Module, Rest};
find_command(_CommandParserFsm, _Rest, unsuccessful_parsing) -> {false, unknown_command};
find_command(_CommandParserFsm, "", ambiguous_parsing) -> {false, ambiguous_command};
find_command(_CommandParserFsm, "", incomplete_parsing) -> {false, incomplete_command};
find_command(CommandParserFsm, Rest, _Result) ->
    {Token, NewRest} = commandline_parser:get_first_token(Rest),
    Result = command_parser_fsm:process_token(CommandParserFsm, Token),
    find_command(CommandParserFsm, NewRest, Result).

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
