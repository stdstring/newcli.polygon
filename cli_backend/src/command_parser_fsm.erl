%% @author stdstring

-module(command_parser_fsm).

-behaviour(gen_fsm).

-include("common_defs.hrl").

-record(command_parser_state, {commands = [] :: [{CommandName :: atom(), CommandModule :: atom()}], recognized_parts = [] :: [string()]}).

%% ====================================================================
%% API functions
%% ====================================================================

-export([start/1, process_token/2, stop/1]).
%% gen_fsm
-export([ambiguous_parsing/3, incomplete_parsing/3, successful_parsing/3, unsuccessful_parsing/3]).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-spec start(KnownCommands :: [{CommandName :: atom(), CommandModule :: atom()}]) -> {'ok', pid()} | {'error', Error :: term()}.
start(KnownCommands) -> gen_fsm:start_link(?MODULE, KnownCommands, []).

-spec process_token(ParserPid :: pid(), Token :: string() | 'eol') -> term().
process_token(ParserPid, eol) ->
    gen_fsm:sync_send_event(ParserPid, eol);
process_token(ParserPid, Token) ->
    gen_fsm:sync_send_event(ParserPid, Token).

stop(ParserPid) -> gen_fsm:send_all_state_event(ParserPid, shutdown).

init(KnownCommands) ->
    {ok, ambiguous_parsing, #command_parser_state{commands = KnownCommands}}.

ambiguous_parsing(Token, _From, #command_parser_state{commands = Commands, recognized_parts = RecognizedParts}) ->
    NewParts = RecognizedParts ++ [Token],
    {Reply, State, StateData} = process_filter(Commands, NewParts),
    {reply, Reply, State, StateData}.

incomplete_parsing(Token, _From, #command_parser_state{commands = [{Name, Module}], recognized_parts = RecognizedParts}) ->
    NewParts = RecognizedParts ++ [Token],
    CommandBody = apply(Module, get_command_body, []),
    if
        CommandBody == NewParts ->
            Reply = #successful_parse_result{command = {Name, Module}, can_continue = false},
            {reply, Reply, successful_parsing, #command_parser_state{commands = [{Name, Module}], recognized_parts = NewParts}};
        true ->
            case lists:prefix(NewParts, CommandBody) of
                true ->
                    Reply = #incomplete_parse_result{},
                    {reply, Reply, incomplete_parsing, #command_parser_state{commands = [{Name, Module}], recognized_parts = NewParts}};
                false ->
                    Reply = #unsuccessful_parse_result{},
                    {reply, Reply, unsuccessful_parsing, #command_parser_state{commands = [], recognized_parts = RecognizedParts}}
            end
    end.

successful_parsing(Token, _From, #command_parser_state{commands = Commands, recognized_parts = RecognizedParts}) ->
    NewParts = RecognizedParts ++ [Token],
    {Reply, State, StateData} = process_filter(Commands, NewParts),
    {reply, Reply, State, StateData}.

unsuccessful_parsing(_Token, _From, StateData) ->
    Reply = #unsuccessful_parse_result{},
    {reply, Reply, unsuccessful_parsing, StateData}.

handle_event(shutdown, _StateName, StateData) ->
    {stop, normal, StateData}.

handle_sync_event(_Event, _From, _StateName, StateData) -> {stop, enotsup, not_supported, StateData}.

handle_info(_Info, StateName, StateData) -> {next_state, StateName, StateData}.

terminate(_Reason, _StateName, _StatData) -> ok.

code_change(_OldVsn, StateName, StateData, _Extra) -> {ok, StateName, StateData}.

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec filter_commands(Commands :: [{CommandName :: atom(), CommandModule :: atom()}], Tokens :: [string()]) ->
          {[{CommandName :: atom(), CommandModule :: atom()}], {CommandName :: atom(), CommandModule :: atom()}}.
filter_commands(Commands, Tokens) ->
    filter_commands(Commands, Tokens, [], undefined).

-spec filter_commands(Commands :: [{CommandName :: atom(), CommandModule :: atom()}],
                      Tokens :: [string()],
                      FilteredCommands :: [{CommandName :: atom(), CommandModule :: atom()}],
                      RecognizedCommand :: {CommandName :: atom(), CommandModule :: atom()} | 'undefined') ->
          {[{CommandName :: atom(), CommandModule :: atom()}], {CommandName :: atom(), CommandModule :: atom()}}.
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

-spec process_filter(Commands :: [{CommandName :: atom(), CommandModule :: atom()}], Tokens :: [string()]) ->
          {Result :: #ambiguous_parse_result{} | #incomplete_parse_result{} | #unsuccessful_parse_result{} | #successful_parse_result{},
           State :: atom(),
           StateData :: #command_parser_state{}}.
process_filter(Commands, Tokens) ->
    case filter_commands(Commands, Tokens) of
        {[], undefined} ->
            Result = #unsuccessful_parse_result{},
            {Result, unsuccessful_parsing, #command_parser_state{commands = []}};
        {[{Name, Module}], undefined} ->
            Result = #incomplete_parse_result{},
            {Result, incomplete_parsing, #command_parser_state{commands = [{Name, Module}], recognized_parts = Tokens}};
        {NewCommands, undefined} ->
            Result = #ambiguous_parse_result{},
            {Result, ambiguous_parsing, #command_parser_state{commands = NewCommands, recognized_parts = Tokens}};
        {NewCommands, {Name, Module}} ->
            CanContinue = length(NewCommands) > 1,
            Result = #successful_parse_result{command = {Name, Module}, can_continue = CanContinue},
            {Result, successful_parsing, #command_parser_state{commands = NewCommands, recognized_parts = Tokens}}
    end.
