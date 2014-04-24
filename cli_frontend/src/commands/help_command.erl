%% @author std-string

-module(help_command).

-behaviour(command_behaviour).

-include("common_defs.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

-export([get_name/0, get_command_body/0, execute/2]).

-spec get_name() -> atom().
get_name() -> help_command.

-spec get_command_body() -> [string()].
get_command_body() -> ["?"].

%% -spec create(CommandLineRest :: string()) -> {'ok', Command :: pid()} | {'error', Reason :: term()}.
%% create(CommandLineRest) -> {error, not_implemented}.

-spec execute(CommandLineRest :: string(), ExecutionState :: #execution_state{}) -> {ReturnCode :: integer(), ExecutionState :: #execution_state{}}.
execute(CommandLineRest, ExecutionState) ->
    SourceCommands = ExecutionState#execution_state.commands_info,
    HasTrailingSpace = lists:suffix(" ", CommandLineRest),
    if
        CommandLineRest == "" ->
            show_commands(SourceCommands);
        HasTrailingSpace == false ->
            Tokens = commandline_parser:get_tokens(CommandLineRest),
            FilteredCommands = filter_commands(SourceCommands, Tokens),
            show_commands(FilteredCommands);
        HasTrailingSpace == true ->
            Tokens = commandline_parser:get_tokens(CommandLineRest),
            Command = find_command(SourceCommands, Tokens),
            show_command(Command)
    end,
    {0, ExecutionState}.

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec find_command(Commands :: [{CommandName :: atom(), CommandBody :: [string()], CommandHelp :: string()}], CommandPrefix :: [string()]) ->
          {CommandName :: atom(), CommandBody :: [string()], CommandHelp :: string()} | 'false'.
find_command(Commands, CommandPrefix) ->
    case lists:filter(fun({_Name, Body, _Help}) -> CommandPrefix == Body end, Commands) of
        [{CommandName, CommandBody, CommandHelp}] -> {CommandName, CommandBody, CommandHelp};
        [] -> false
    end.

-spec filter_command(CommandBody :: [string()], Prefix :: [string()]) -> boolean().
filter_command([], _PrefixRest) -> false;
filter_command([CommandBodyPart | _CommandBodyRest], [PartialPrefixPart]) ->
    lists:prefix(PartialPrefixPart, CommandBodyPart);
filter_command([CommandBodyPart | CommandBodyRest], [PrefixPart | PrefixRest]) ->
    case CommandBodyPart == PrefixPart of
        true -> filter_command(CommandBodyRest, PrefixRest);
        false -> false
    end.

-spec filter_commands(Commands :: [{CommandName :: atom(), CommandBody :: [string()], CommandHelp :: string()}], CommandPrefix :: [string()]) ->
          [{CommandName :: atom(), CommandBody :: [string()], CommandHelp :: string()}].
filter_commands(Commands, CommandPrefix) ->
    lists:filter(fun({_Name, Body, _Help}) -> filter_command(Body, CommandPrefix) end, Commands).

join_command_body(CommandBodyParts) ->
    %% separator == space
    string:join(CommandBodyParts, " ").

-spec show_commands(Commands :: [{CommandName :: atom(), CommandBody :: [string()], CommandHelp :: string()}]) -> 'ok'.
show_commands([]) ->
    io:format("Commands not found.~n"),
    ok;
show_commands(Commands) ->
    CommandBodyList = lists:map(fun({_Name, Body, _Help}) -> join_command_body(Body) end, Commands),
    io:format("~s~n", [string:join(CommandBodyList, "\t")]),
    ok.

-spec show_command(Command :: {CommandName :: atom(), CommandBody :: [string()], CommandHelp :: string()} | 'false') -> 'ok'.
show_command(false) ->
    io:format("Help not found.~n"),
    ok;
show_command({_Name, _Body, Help}) ->
    io:format("~s", [string_data_utils:add_trailing_line_feed(Help)]),
    ok.
