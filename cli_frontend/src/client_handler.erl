-module(client_handler).

-behaviour(gen_server).

-include("common_defs.hrl").

-define(DEVICE_NAME, "CliDemo").

%% ====================================================================
%% API functions
%% ====================================================================

-export([start/1, process_command/2, interrupt_command/1, get_current_state/1, get_extensions/2, exit/1, send_output/2, send_error/2, finish_command/3]).
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

-spec send_output(Handler :: pid(), Output :: string()) -> 'ok'.
send_output(Handler, Output) ->
    gen_server:cast(Handler, {command_out, Output}).

-spec send_error(Handler :: pid(), Error :: string()) -> 'ok'.
send_error(Handler, Error) ->
    gen_server:cast(Handler, {command_err, Error}).

-spec finish_command(Handler :: pid(), ExecutionState :: #execution_state{}, ReturnCode :: integer()) -> ok.
finish_command(Handler, ExecutionState, ReturnCode) ->
    gen_server:cast(Handler, {command_end, ExecutionState, ReturnCode}).

%%exit(Handler) -> gen_server:cast(Handler, exit).
exit(_Handler) -> ok.

init(_Args) ->
    GlobalConfig = #global_config{},
    ExecutionState = #execution_state{device_name = ?DEVICE_NAME},
    State = #client_handler_state{config = GlobalConfig, execution_state = ExecutionState},
    {ok, State}.

handle_call({process_command, CommandLine}, _From, #client_handler_state{command_chain = []} = State) ->
    GlobalConfig = State#client_handler_state.config,
    CommandChain = command_parser:parse(CommandLine, GlobalConfig),
    ExecutionState = State#client_handler_state.execution_state,
    Request = {command_end, ExecutionState, 0},
    NewState = command_execution_context:process(Request, State#client_handler_state{command_chain = CommandChain}),
    {reply, true, NewState};
handle_call({process_command, _CommandLine}, _From, State) ->
    {reply, false, State};
handle_call(current_state, _From, State) ->
    Prompt = prompt_factory:generate_prompt(State#client_handler_state.execution_state),
    {reply, Prompt, State};
handle_call({extensions, CommandLine}, _From, State) ->
    ExtensionGenerator = State#client_handler_state.extension_generator,
    Extensions = ExtensionGenerator(CommandLine),
    {reply, Extensions, State}.

handle_cast(Request, State) ->
    NewState = command_execution_context:process(Request, State),
    {noreply, NewState}.

handle_info(_Info, State) -> {stop, enotsup, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================