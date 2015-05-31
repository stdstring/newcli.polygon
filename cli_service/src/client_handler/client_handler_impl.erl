%% @author std-string

-module(client_handler_impl).

-behaviour(gen_server).

-include("authentication_defs.hrl").
-include("cli_fsm_defs.hrl").
-include("client_handler_defs.hrl").
-include("command_defs.hrl").
-include("common_defs.hrl").

%% gen_server export
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================

init(_Args) -> {stop, enotsup}.

handle_call(#process_command{command_line = CommandLine}, _From, #client_handler_state{current_command = undefined} = State) ->
    IntermediateState = client_downtime_timer:stop(State),
    GlobalConfig = IntermediateState#client_handler_state.config,
    %% TODO (std_string) : think about caching
    LexConfig = lex_analyzer_config:create(true),
    NameConfig = name_search_config:create(GlobalConfig#global_config.commands),
    SyntaxConfig = syntax_analyzer_config:create(NameConfig),
    CommandModule = State#client_handler_state.command_module,
    case command_factory:process(CommandLine, LexConfig, SyntaxConfig, GlobalConfig, CommandModule) of
        {true, CommandFun} ->
            FinishState = process_start_command(IntermediateState, CommandFun),
            {reply, true, FinishState};
        {false, Reason} ->
            FinishState = process_command_creation_error(State, Reason),
            {reply, false, FinishState}
    end;
handle_call(#process_command{}, _From, State) ->
    command_helper:send_error(State, ?COMMAND_ALREADY_RUN_MESSAGE),
    {reply, false, State};
handle_call(#get_current_state{}, _From, State) ->
    Prompt = prompt_factory:generate_prompt(State),
    NewState = client_downtime_timer:restart(State),
    {reply, Prompt, NewState};
handle_call(#get_extensions{command_line = CommandLine}, _From, State) ->
    {Prefix, Commands} = client_handler_helper:get_suitable_commands(CommandLine, State),
    NewState = client_downtime_timer:restart(State),
    {reply, {Prefix, Commands}, NewState};
handle_call(#get_help{command_line = CommandLine}, _From, State) ->
    Help = client_handler_helper:get_help(CommandLine, State),
    NewState = client_downtime_timer:restart(State),
    {reply, Help, NewState};
handle_call(#get_suitable_commands{command_line = CommandLine}, _From, State) ->
    {_Prefix, Commands} = client_handler_helper:get_suitable_commands(CommandLine, State),
    NewState = client_downtime_timer:restart(State),
    {reply, Commands, NewState};
handle_call(#current_mode_exit{}, _From, State) ->
    {ExecutionState, Prompt, IntermediateState} = process_current_mode_exit(State),
    NewState = client_downtime_timer:restart(IntermediateState),
    {reply, {ExecutionState, Prompt}, NewState}.

handle_cast(#client_exit{}, State) ->
    NewState = client_downtime_timer:stop(State),
    {noreply, NewState};
handle_cast(Request, State) ->
    NewState = command_executor:process(Request, State),
    {noreply, NewState}.

%% command's normal exit
handle_info({'EXIT', _From, normal}, State) -> {noreply, State};
%% command's interrupt
handle_info({'EXIT', _From, interrupt}, State) -> {noreply, State};
%% command's abnormal exit
handle_info({'EXIT', _From, _Other}, State) -> {stop, command_exit, State};
%% timer
handle_info({timeout, TimerRef, downtime}, State) ->
    case State#client_handler_state.timer_ref of
        TimerRef ->
            NewState = State#client_handler_state{timer_ref = undefined},
            Endpoint = NewState#client_handler_state.endpoint,
            cli_terminal_endpoint:stop(Endpoint, downtime),
            {noreply, NewState};
        _Other -> {noreply, State}
    end;
%% other info
handle_info(_Info, State) -> {stop, enotsup, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec process_start_command(State :: #client_handler_state{},
                            CommandFun :: fun((CliFsm :: pid(), ClientHandler :: pid(), Context :: [{Key :: atom(), Value :: term()}]) -> 'ok')) ->
    #client_handler_state{}.
process_start_command(State, CommandFun) ->
    ExecutionContext = execution_context_factory:create(State),
    CliFsm = State#client_handler_state.cli_fsm,
    ClientHandler = self(),
    Command = spawn_link(fun() -> CommandFun(CliFsm, ClientHandler, ExecutionContext) end),
    State#client_handler_state{current_command = Command}.

-spec process_command_creation_error(State ::  #client_handler_state{}, Reason :: term()) ->
    #client_handler_state{}.
process_command_creation_error(State, Reason) ->
    Error = string_utils:format(?COMMAND_CREATION_ERROR_TEMPLATE, [Reason]),
    command_helper:send_error(State, Error),
    command_helper:send_end(State, ?EX_CONTINUE),
    client_downtime_timer:start(State).

process_current_mode_exit(State) ->
    CliFsm = State#client_handler_state.cli_fsm,
    CliFsmInfo = cli_fsm:get_current_state(CliFsm),
    ExitCommand = CliFsmInfo#cli_fsm_state_info.exit_command,
    %% TODO (std_string) : process failure case
    {ok, IoBuffer} = null_io_buffer:start(),
    {ExecutionState, NewStateStage} = command_sync_executer:execute(ExitCommand, [], State, IoBuffer, null_io_buffer),
    case ExecutionState of
        ?EX_STOP -> {ExecutionState, "", NewStateStage};
        ?EX_CONTINUE -> {ExecutionState, prompt_factory:generate_prompt(State), NewStateStage}
    end.