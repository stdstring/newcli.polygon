%% @author stdstring

-module(command_parser_fsm).

-behaviour(gen_fsm).

-include("common_defs.hrl").

-record(state, {commands = [] :: [{CommandName :: atom(), CommandModule :: atom()}], recognized_parts = [] :: [string()]}).

%% ====================================================================
%% API functions
%% ====================================================================

-export([start/1, process_token/2]).
%% gen_fsm
-export([ambiguous_parsing/3, incomplete_parsing/3, successful_parsing/3, unsuccessful_parsing/3]).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-spec start(KnownCommands :: [{CommandName :: atom(), CommandModule :: atom()}]) -> pid() | {'error', Error :: term()}.
start(KnownCommands) -> gen_fsm:start_link(?MODULE, KnownCommands, []).

-spec process_token(ParserPid :: pid(), Token :: string() | 'eol') -> term().
process_token(ParserPid, eol) ->
    gen_fsm:sync_send_event(ParserPid, eol);
process_token(ParserPid, Token) ->
    gen_fsm:sync_send_event(ParserPid, Token).

init(KnownCommands) ->
    {ok, ambiguous_parsing, #state{commands = KnownCommands}}.

ambiguous_parsing(Token, _From, #state{commands = Commands, recognized_parts = RecognizedParts}) ->
    NewParts = RecognizedParts ++ [Token],
    case filter_commands(Commands, NewParts) of
        {[], undefined} ->
            Reply = #parse_result{state = unsuccessful_parsing},
            {reply, Reply, unsuccessful_parsing, #state{commands = [], recognized_parts = RecognizedParts}};
        {[{Name, Module}], undefined} ->
            Reply = #parse_result{state = incomplete_parsing, can_continue = true},
            {reply, Reply, incomplete_parsing, #state{commands = [{Name, Module}], recognized_parts = NewParts}};
        {NewCommands, undefined} ->
            Reply = #parse_result{state = ambiguous_parsing, can_continue = true},
            {reply, Reply, ambiguous_parsing, #state{commands = NewCommands, recognized_parts = NewParts}};
        {NewCommands, {Name, Module}} ->
            CanContinue = length(NewCommands) > 1,
            Reply = #parse_result{state = successful_parsing, command = {Name, Module}, can_continue = CanContinue},
            {reply, Reply, successful_parsing, #state{commands = NewCommands, recognized_parts = NewParts}}
    end.

incomplete_parsing(Token, _From, #state{commands = [{Name, Module}], recognized_parts = RecognizedParts}) ->
    NewParts = RecognizedParts ++ [Token],
    CommandBody = apply(Module, get_command_body, []),
    if
        CommandBody == NewParts ->
            Reply = #parse_result{state = successful_parsing, command = {Name, Module}, can_continue = false},
            {reply, Reply, successful_parsing, #state{commands = [{Name, Module}], recognized_parts = NewParts}};
        true ->
            case lists:prefix(NewParts, CommandBody) of
                true ->
                    Reply = #parse_result{state = incomplete_parsing, can_continue = true},
                    {reply, Reply, incomplete_parsing, #state{commands = [{Name, Module}], recognized_parts = NewParts}};
                false ->
                    Reply = #parse_result{state = unsuccessful_parsing},
                    {reply, Reply, unsuccessful_parsing, #state{commands = [], recognized_parts = RecognizedParts}}
            end
    end.

successful_parsing(Token, _From, #state{commands = Commands, recognized_parts = RecognizedParts}) ->
    NewParts = RecognizedParts ++ [Token],
    case filter_commands(Commands, NewParts) of
        {[], undefined} ->
            Reply = #parse_result{state = unsuccessful_parsing},
            {reply, Reply, unsuccessful_parsing, #state{commands = [], recognized_parts = RecognizedParts}};
        {[{Name, Module}], undefined} ->
            Reply = #parse_result{state = incomplete_parsing, can_continue = true},
            {reply, Reply, incomplete_parsing, #state{commands = [{Name, Module}], recognized_parts = NewParts}};
        {NewCommands, undefined} ->
            Reply = #parse_result{state = ambiguous_parsing, can_continue = true},
            {reply, Reply, ambiguous_parsing, #state{commands = NewCommands, recognized_parts = NewParts}};
        {NewCommands, {Name, Module}} ->
            CanContinue = length(NewCommands) > 1,
            Reply = #parse_result{state = successful_parsing, command = {Name, Module}, can_continue = CanContinue},
            {reply, Reply, successful_parsing, #state{commands = NewCommands, recognized_parts = NewParts}}
    end.

unsuccessful_parsing(_Token, _From, StateData) ->
    Reply = #parse_result{state = unsuccessful_parsing},
    {reply, Reply, unsuccessful_parsing, StateData}.

handle_event(_Event, _StateName, StateData) -> {stop, not_supported, StateData}.

handle_sync_event(_Event, _From, _StateName, StateData) -> {stop, not_supported, not_supported, StateData}.

handle_info(_Info, StateName, StateData) -> {next_state, StateName, StateData}.

terminate(_Reason, _StateName, _StatData) -> ok.

code_change(_OldVsn, StateName, StateData, _Extra) -> {ok, StateName, StateData}.

%% ====================================================================
%% Internal functions
%% ====================================================================

filter_commands(Commands, Tokens) ->
    filter_commands(Commands, Tokens, [], undefined).

filter_commands([], _Tokens, FilteredCommands, RecognizedCommand) -> {FilteredCommands, RecognizedCommand};
filter_commands([{CommandName, CommandModule} | CommandsRest], Tokens, FilteredCommands, RecognizedCommand) ->
    CommandBody = apply(CommandModule, get_command_body, []),
    if
        CommandBody == Tokens ->
            filter_commands(CommandsRest, Tokens, [{CommandName, CommandModule}] ++ FilteredCommands, {CommandName, CommandModule});
        true ->
            case lists:prefix(Tokens, CommandBody) of
                true -> filter_commands(CommandsRest, Tokens, [{CommandName, CommandModule}] ++ FilteredCommands, RecognizedCommand);
                false -> filter_commands(CommandsRest, Tokens, FilteredCommands, RecognizedCommand)
            end
    end.

