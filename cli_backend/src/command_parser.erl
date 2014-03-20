%% @author std-string

-module(command_parser).

-include("common_defs.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

-export([parse/3]).

-spec parse(CommandLine :: string(), Config :: #config{}, ClientOutput :: pid()) -> [pid()] | false.
parse(CommandLine, Config, ClientOutput) when is_record(Config, config) ->
    Commands = Config#config.commands,
    CommandLineParts = commandline_parser:parse(CommandLine),
    case find_command(CommandLineParts, Commands) of
        {_, CommandModule} -> create_commands(CommandModule, CommandLineParts, ClientOutput);
        false -> {command_parser, unknown_command}
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec find_command(CommandLineParts :: [string()], Commands :: [{CommandName :: atom(), CommandModule :: atom()}]) -> {CommandName :: atom(), CommandModule :: atom()} | false.
find_command(CommandLineParts, Commands) ->
    RecognizedCommands = recognize_commands(CommandLineParts, Commands),
    select_best_command(RecognizedCommands).

-spec recognize_commands(CommandLineParts :: [string()], Commands :: [{CommandName :: atom(), CommandModule :: atom()}]) -> [{CommandName :: atom(), CommandModule :: atom()}].
recognize_commands(CommandLineParts, Commands) ->
    lists:filter(fun({_, Module}) -> lists:prefix(apply(Module, get_command_body, []), CommandLineParts) end, Commands).

-spec select_best_command(Commands :: [{CommandName :: atom(), CommandModule :: atom()}]) -> {CommandName :: atom(), CommandModule :: atom()} | false.
select_best_command([]) -> false;
select_best_command([Command]) -> Command;
select_best_command([FirstCommand | Commands]) ->
    lists:foldl(fun(Command, Acc) ->
                        CommandBody = apply(element(2, Command), get_command_body, []),
                        AccBody = apply(element(2, Acc), get_command_body, []),
                        Condition = length(CommandBody) > length(AccBody),
                        logic_utils:ternary_op(Condition, Acc, Command)
                end, FirstCommand, Commands).

-spec create_commands(CommandModule :: atom(), CommandLineParts :: [string()], ClientOutput :: pid()) -> [pid()].
create_commands(CommandModule, CommandLineParts, ClientOutput) ->
    case output_endpoint:start(ClientOutput) of
        {output_endpoint, Error} -> {output_endpoint, Error};
        OutputEndpointPid ->
            Stdout = OutputEndpointPid,
            Stderr = OutputEndpointPid,
            case apply(CommandModule, create, [CommandLineParts, Stdout, Stderr]) of
                {error, Reason} ->
                    CommandName = apply(CommandModule, get_name, []),
                    {CommandName, {creation_error, Reason}};
                CommandPid -> [CommandPid, OutputEndpointPid]
            end
    end.
