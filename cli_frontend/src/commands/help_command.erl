%% @author std-string

-module(help_command).

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
execute(_CommandLineRest, ExecutionState) -> {0, ExecutionState}.

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

%%filter_commands(Commands, CommandPrefix) ->
%%    {FullParts, PartialPart} = lists:split(length(CommandPrefix) - 1, CommandPrefix),
%%    PartialPartIndex = length(CommandPrefix),
%%    lists:filter(fun({_Name, Body, _Help}) -> lists end, Commands)

get_command_body(CommandBodyParts) ->
    %% separator == space
    string:join(CommandBodyParts, " ").