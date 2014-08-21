%% @author std-string

-module(help_command).

-behaviour(command_behaviour).

-include("common_defs.hrl").

-define(COMMANDS_NOT_FOUND, "Commands not found\n").
-define(HELP_NOT_FOUND, "Help not found\n").

%% ====================================================================
%% API functions
%% ====================================================================

-export([get_name/0, get_command_body/0, execute/3]).
%% proc_lib export
-export([execute_impl/3]).

-spec get_name() -> atom().
get_name() -> help_command.

-spec get_command_body() -> [string()].
get_command_body() -> ["?"].

-spec execute(CommandLineRest :: string(), ClientHandler :: pid(), ExecutionState :: #execution_state{}) -> CommandPid :: pid().
execute(CommandLineRest, ClientHandler, ExecutionState) ->
    proc_lib:start_link(?MODULE, execute_impl, [CommandLineRest, ClientHandler, ExecutionState]).

-spec execute_impl(CommandLineRest :: string(), ClientHandler :: pid(), ExecutionState :: #execution_state{}) -> 'ok'.
execute_impl(CommandLineRest, ClientHandler, ExecutionState) ->
    SourceCommands = ExecutionState#execution_state.commands_info,
    HasTrailingSpace = lists:suffix(" ", CommandLineRest),
    if
        CommandLineRest == "" ->
            show_commands(SourceCommands, ClientHandler);
        HasTrailingSpace == false ->
            Tokens = commandline_parser:get_tokens(CommandLineRest),
            FilteredCommands = filter_commands(SourceCommands, Tokens),
            show_commands(FilteredCommands, ClientHandler);
        HasTrailingSpace == true ->
            Tokens = commandline_parser:get_tokens(CommandLineRest),
            Command = find_command(SourceCommands, Tokens),
            show_command(Command, ClientHandler)
    end,
    client_handler:finish_command(ClientHandler, ExecutionState, 0),
    ok.

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
filter_command([Part | CommandBodyRest], [Part | PrefixRest]) ->
    filter_command(CommandBodyRest, PrefixRest);
filter_command(_CommandBody, _Prefix) ->
    false.

-spec filter_commands(Commands :: [{CommandName :: atom(), CommandBody :: [string()], CommandHelp :: string()}], CommandPrefix :: [string()]) ->
    [{CommandName :: atom(), CommandBody :: [string()], CommandHelp :: string()}].
filter_commands(Commands, CommandPrefix) ->
    lists:filter(fun({_Name, Body, _Help}) -> filter_command(Body, CommandPrefix) end, Commands).

-spec join_command_body(CommandBodyParts :: [string()]) -> string().
join_command_body(CommandBodyParts) ->
    %% separator == space
    string:join(CommandBodyParts, " ").

-spec show_commands(Commands :: [{CommandName :: atom(), CommandBody :: [string()], CommandHelp :: string()}], ClientHandler :: pid()) -> 'ok'.
show_commands([], ClientHandler) ->
    client_handler:send_output(ClientHandler, ?COMMANDS_NOT_FOUND),
    ok;
show_commands(Commands, ClientHandler) ->
    CommandBodyList = lists:map(fun({_Name, Body, _Help}) -> join_command_body(Body) end, Commands),
    Message = string_data_utils:add_trailing_line_feed(string:join(CommandBodyList, "\t")),
    client_handler:send_output(ClientHandler, Message),
    ok.

-spec show_command(Command :: {CommandName :: atom(), CommandBody :: [string()], CommandHelp :: string()} | 'false', ClientHandler :: pid()) -> 'ok'.
show_command(false, ClientHandler) ->
    client_handler:send_output(ClientHandler, ?HELP_NOT_FOUND),
    ok;
show_command({_Name, _Body, Help}, ClientHandler) ->
    Message = string_data_utils:add_trailing_line_feed(Help),
    client_handler:send_output(ClientHandler, Message),
    ok.
