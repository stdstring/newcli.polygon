-module(client_handler).

-behaviour(gen_server).

-include("common_defs.hrl").

-define(DEVICE_NAME, "CliDemo").

-record(client_handler_state, {config :: #global_config{},
                               execution_state :: #execution_state{},
                               execution_context = undefined :: 'undefined' | pid(),
                               endpoint = undefined :: 'undefined' | pid(),
                               extension_generator = undefined :: 'undefined' | fun((string()) -> [string()])}).

%% ====================================================================
%% API functions
%% ====================================================================

-export([start/1, process_command/2, interrupt_command/1, get_current_state/1, get_extensions/2, exit/1]).
%% gen_server export
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start(Socket) ->
    gen_server:start_link(?MODULE, Socket, []).

-spec process_command(Handler :: pid(), CommandLine :: string()) -> boolean().
process_command(Handler, CommandLine) ->
    gen_server:call(Handler, {process_command, CommandLine}).

-spec interrupt_command(Handler :: pid()) -> 'ok'.
interrupt_command(Handler) ->
    gen_server:cast(Handler, interrupt_command).

-spec get_current_state(Handler :: pid()) -> string().
get_current_state(Handler) ->
    gen_server:call(Handler, current_state).

-spec get_extensions(Handler :: pid(), CommandLine :: string()) -> [string()].
get_extensions(Handler, CommandLine) ->
    gen_server:call(Handler, {extensions, CommandLine}).

%%exit(Handler) -> gen_server:cast(Handler, exit).
exit(_Handler) -> ok.

init(_Args) ->
    GlobalConfig = #global_config{},
    ExecutionState = #execution_state{device_name = ?DEVICE_NAME},
    State = #client_handler_state{config = GlobalConfig, execution_state = ExecutionState},
    {ok, State}.

handle_call({process_command, _CommandLine}, _From, #client_handler_state{execution_context = undefined} = State) ->
    {reply, true, State};
handle_call({process_command, _CommandLine}, _From, State) ->
    {reply, false, State};
handle_call(current_state, _From, State) ->
    Prompt = prompt_factory:generate_prompt(State#client_handler_state.execution_state),
    {reply, Prompt, State};
handle_call({extensions, CommandLine}, _From, State) ->
    ExtensionGenerator = State#client_handler_state.extension_generator,
    Extensions = ExtensionGenerator(CommandLine),
    {reply, Extensions, State}.

handle_cast(interrupt_command, State) -> {stop, enotsup, State}.

handle_info(_Info, State) -> {stop, enotsup, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================