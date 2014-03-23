%% @author std-string

-module(command_parser).

-include("common_defs.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

-export([parse/3]).

-spec parse(CommandLine :: string(), Config :: #config{}, ClientOutput :: pid()) -> [pid()] | 'false'.
parse(CommandLine, Config, ClientOutput) when is_record(Config, config) ->
    Commands = Config#config.commands,
    case find_command(CommandLine, Commands) of
        {_, CommandModule, CommandLineRest} -> create_command_chain(CommandModule, CommandLineRest, ClientOutput);
        false -> {command_parser, unknown_command}
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec find_command(CommandLine :: string(), Commands :: [{CommandName :: atom(), CommandModule :: atom()}]) ->
          {CommandName :: atom(), CommandModule :: atom(), ComandLineRest :: string()} | 'false'.
find_command(CommandLine, Commands) ->
    {[FirstToken], Rest} = commandline_parser:get_tokens(CommandLine, 1),
    case find_command(FirstToken, Rest, Commands, undefined) of
        {Name, Module, _TokensCount, OtherRest} -> {Name, Module, OtherRest};
        false -> false
    end.

-spec find_command(FirstToken :: string(),
                   Rest :: string(),
                   Commands :: [{CommandName :: atom(), CommandModule :: atom()}],
                   BestCommand :: {Name :: atom(), Module  :: atom(), TokensCount :: integer(), OtherRest :: string()} | 'undefined') ->
          {CommandName :: atom(), CommandModule :: atom(), ComandLineRest :: string()} | 'false'.
find_command(_FirstToken, _Rest, [], undefined) -> false;
find_command(_FirstToken, _Rest, [], BestCommand) -> BestCommand;
find_command(FirstToken, Rest, Commands, undefined) ->
    [{_Name, Module} | _CommandsRest] = Commands,
    CommandBody = apply(Module, get_command_body, []),
    apply_check(FirstToken, Rest, CommandBody, Commands, undefined);
find_command(FirstToken, Rest, Commands, BestCommand) ->
    [{_Name, Module} | _CommandsRest] = Commands,
    {_Name, _Module, BestCount, _OtherRest} = BestCommand,
    CommandBody = apply(Module, get_command_body, []),
    CommandBodyLength = length(CommandBody),
    if
        CommandBodyLength > BestCount -> apply_check(FirstToken, Rest, CommandBody, Commands, BestCommand);
        CommandBodyLength =< BestCount -> find_command(FirstToken, Rest, Commands, BestCommand)
    end.

-spec apply_check(FirstToken :: string(),
                  Rest :: string(),
                  CommandBody :: [string()],
                  Commands :: [{CommandName :: atom(), CommandModule :: atom()}],
                  OldBestCommand :: {Name :: atom(), Module  :: atom(), TokensCount :: integer(), OtherRest :: string()} | 'undefined') -> no_return().
apply_check(FirstToken, Rest, CommandBody, Commands, OldBestCommand) ->
    [{Name, Module} | CommandsRest] = Commands,
    case check_command(FirstToken, Rest, CommandBody) of
        {true, TokensCount, OtherRest} -> find_command(FirstToken, Rest, CommandsRest, {Name, Module, TokensCount, OtherRest});
        false -> find_command(FirstToken, Rest, CommandsRest, OldBestCommand)
    end.

-spec check_command(FirstToken :: string(), Rest :: string(), Body :: [string()]) -> {'true', TokenCount :: integer, OtherRest :: string()} | 'false'.
check_command(FirstToken, _Rest, [FirstWord | _Words]) when FirstToken /= FirstWord -> false;
check_command(FirstToken, Rest, Body) ->
    [FirstToken | Words] = Body,
    {OtherTokens, OtherRest} = commandline_parser:get_tokens(Rest, length(Words)),
    Tokens = [FirstToken] ++ OtherTokens,
    logic_utils:ternary_op(Tokens == Body, {true, length(Tokens), OtherRest}, false).

-spec create_command_chain(CommandModule :: atom(), CommandLineRest :: string(), ClientOutput :: pid()) -> [pid()].
create_command_chain(CommandModule, CommandLineRest, ClientOutput) ->
    case output_endpoint:start(ClientOutput) of
        {output_endpoint, Error} -> {output_endpoint, Error};
        OutputEndpointPid ->
            Stdout = OutputEndpointPid,
            Stderr = OutputEndpointPid,
            case apply(CommandModule, create, [CommandLineRest, Stdout, Stderr]) of
                {error, Reason} ->
                    CommandName = apply(CommandModule, get_name, []),
                    {CommandName, {creation_error, Reason}};
                CommandPid -> [CommandPid, OutputEndpointPid]
            end
    end.
