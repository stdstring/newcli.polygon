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
        false -> false
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
    Comparator = fun({_1, Module1}, {_2, Module2}) ->
                         length(apply(Module1, get_command_body, [])) > length(apply(Module2, get_command_body, []))
                 end,
    lists:foldl(fun(Command, Acc) -> logic_utils:ternary_op(Comparator, Acc, Command) end, FirstCommand, Commands).

-spec create_commands(CommandModule :: atom(), CommandLineParts :: [string()], ClientOutput :: pid()) -> [pid()].
create_commands(CommandModule, CommandLineParts, ClientOutput) ->
    OutputEndpointPid = output_endpoint:start(ClientOutput),
    Stdout = OutputEndpointPid,
    Stderr = OutputEndpointPid,
    CommandPid = apply(CommandModule, create, [CommandLineParts, Stdout, Stderr]),
    [CommandPid, OutputEndpointPid].
