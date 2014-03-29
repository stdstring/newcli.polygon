%% @author stdstring

-module(command_parser_fsm).

-behaviour(gen_fsm).

-record(state, {commands = [] :: [{CommandName :: atom(), CommandModule :: atom()}], recognized_parts = [] :: [string()]}).

%% ====================================================================
%% API functions
%% ====================================================================

-export([start/1, process_token/2]).
%% gen_fsm
-export([ambiguous_parsing/3, incomplete_parsing/3, successful_parsing/3, unsuccessful_parsing/3]).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-spec start(KnownCommands :: [{CommandName :: atom(), CommandModule :: atom()}]) -> pid() | {'error', Error :: term()}.
start(KnownCommands) ->
    start_fsm(KnownCommands).

-spec process_token(ParserPid :: pid(), Token :: string()) -> term().
process_token(ParserPid, Token) ->
    gen_fsm:sync_send_event(ParserPid, Token).

init(KnownCommands) ->
    {ok, ambiguous_parsing, #state{commands = KnownCommands}}.

ambiguous_parsing(Token, _From, #state{commands = Commands, recognized_parts = RecognizedParts}) ->
    NewParts = RecognizedParts ++ [Token],
    NewCommands = lists:filter(fun({_, Module}) -> lists:prefix(NewParts, apply(Module, get_command_body, [])) end, Commands),
    case NewCommands of
        [] -> {reply, unsuccessful_parsing, unsuccessful_parsing, #state{commands = NewCommands, recognized_parts = NewParts}};
        [{Name, Module}] -> 
            CommandBody = apply(Module, get_command_body, []),
            process_single_command(NewParts, CommandBody, Name, Module);
        _Other -> {reply, ambiguous_parsing, ambiguous_parsing, #state{commands = NewCommands, recognized_parts = NewParts}}
    end.

incomplete_parsing(Token, _From, #state{commands = [{Name, Module}], recognized_parts = RecognizedParts}) ->
    CommandBody = apply(Module, get_command_body, []),
    process_single_command(RecognizedParts ++ [Token], CommandBody, Name, Module).

successful_parsing(_Token, _From, StateData) ->
    {stop, finished_state, finished_state, StateData}.

unsuccessful_parsing(_Event, _From, StateData) ->
    {stop, finished_state, finished_state, StateData}.

handle_event(_Event, _StateName, StateData) -> {stop, not_supported, StateData}.

handle_sync_event(_Event, _From, _StateName, StateData) -> {stop, not_supported, not_supported, StateData}.

handle_info(_Info, StateName, StateData) -> {next_state, StateName, StateData}.

terminate(_Reason, _StateName, _StatData) -> ok.

code_change(_OldVsn, StateName, StateData, _Extra) -> {ok, StateName, StateData}.

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec start_fsm(KnownCommands :: [{CommandName :: atom(), CommandModule :: atom()}]) -> pid() | {'error', Error :: term()}.
start_fsm(KnownCommands) ->
    case gen_fsm:start_link(?MODULE, KnownCommands, []) of
        {ok, Pid} -> Pid;
        {error, Error} -> {command_parser_fsm, Error}
    end.


-spec process_single_command(RecognizedParts :: [string()], CommandBody :: [string()], Name :: atom(), Module :: atom()) ->
          {'reply', Reply :: term(), State :: atom(), StateData :: #state{}}.
process_single_command(RecognizedParts, CommandBody, Name, Module) ->
    IsPrefix = lists:prefix(RecognizedParts, CommandBody),
    if
        RecognizedParts == CommandBody ->
            Reply = {successful_parsing, {Name, Module}},
            {reply, Reply, successful_parsing, #state{commands = [{Name, Module}], recognized_parts = RecognizedParts}};
        IsPrefix ->
            {reply, incomplete_parsing, incomplete_parsing, #state{commands = [{Name, Module}], recognized_parts = RecognizedParts}};
        true ->
            {reply, unsuccessful_parsing, unsuccessful_parsing, #state{commands = [], recognized_parts = RecognizedParts}}
    end.
