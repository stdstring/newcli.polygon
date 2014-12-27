%% @author stdstring

-module(client_handler).

-behaviour(gen_server).

-include("authentication_defs.hrl").
-include("client_handler_defs.hrl").
-include("common_defs.hrl").

%%-define(DEVICE_NAME, "CliDemo").
-define(PARSER_ERROR, "Command's parsing is failed due to the following: ~w\n").

-export([start/2, process_command/2, interrupt_command/1, get_current_state/1, get_extensions/2, exit/1]).
-export([send_output/2, send_error/2, finish_command/3, finish_exec/3]).
%% gen_server export
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================

-spec start(GlobalConfig :: #global_config{}, Endpoint :: pid()) -> {'ok', Pid :: pid()} | {'error', Reason :: term()}.
start(GlobalConfig, Endpoint) ->
    gen_server:start_link(?MODULE, [GlobalConfig, Endpoint], []).

-spec process_command(Handler :: pid(), CommandLine :: string()) -> boolean().
process_command(Handler, CommandLine) ->
    gen_server:call(Handler, {?PROCESS, CommandLine}).

-spec interrupt_command(Handler :: pid()) -> 'ok'.
interrupt_command(Handler) ->
    gen_server:cast(Handler, ?INTERRUPT).

-spec get_current_state(Handler :: pid()) -> string().
get_current_state(Handler) ->
    gen_server:call(Handler, ?CURRENT_STATE).

-spec get_extensions(Handler :: pid(), CommandLine :: string()) -> [string()].
get_extensions(Handler, CommandLine) ->
    gen_server:call(Handler, {?EXTENSIONS, CommandLine}).

-spec exit(Handler :: pid()) -> 'ok'.
exit(Handler) ->
    gen_server:cast(Handler, ?EXIT).

-spec send_output(Handler :: pid(), Output :: string()) -> 'ok'.
send_output(Handler, Output) ->
    gen_server:cast(Handler, {?COMMAND_OUTPUT, Output}).

-spec send_error(Handler :: pid(), Error :: string()) -> 'ok'.
send_error(Handler, Error) ->
    gen_server:cast(Handler, {?COMMAND_ERROR, Error}).

-spec finish_command(Handler :: pid(), ReturnCode :: integer(), ExecutionState :: [{Key :: atom(), Value :: term()}]) -> 'ok'.
finish_command(Handler, ReturnCode, ExecutionState) ->
    gen_server:cast(Handler, {?FINISH_COMMAND, ReturnCode, ExecutionState}).

-spec finish_exec(Handler :: pid(), ReturnCode :: integer(), ExecutionState :: [{Key :: atom(), Value :: term()}]) -> 'ok'.
finish_exec(Handler, ReturnCode, ExecutionState) ->
    gen_server:cast(Handler, {?FINISH_EXEC, ReturnCode, ExecutionState}).

%%init([GlobalConfig, Endpoint]) ->
%%    %% for catching exit signals from commands
%%    process_flag(trap_exit, true),
%%    case create_execution_state(GlobalConfig) of
%%        {ok, ExecutionState} ->
%%            CommandsInfo = ExecutionState#execution_state.commands_info,
%%            CommandsBody = lists:map(fun({_Name, Body, _Help}) -> Body end, CommandsInfo),
%%            Generator = autocomplete_factory:create_extension_generator(CommandsBody),
%%            State = #client_handler_state{config = GlobalConfig, execution_state = ExecutionState, endpoint = Endpoint, extension_generator =  Generator},
%%            {ok, State};
%%        {error, Reason} ->
%%            {stop, Reason}
%%    end.

init([GlobalConfig, Endpoint]) ->
    %% for catching exit signals from commands
    process_flag(trap_exit, true),
    State = #client_handler_state{config = GlobalConfig, endpoint = Endpoint},
    {ok, State}.

%%handle_call({process_command, CommandLine}, _From, #client_handler_state{command_chain = []} = State) ->
%%    GlobalConfig = State#client_handler_state.config,
%%    case command_parser:parse(CommandLine, GlobalConfig) of
%%        {command_parser, Reason} ->
%%            NewState = process_parser_error(State, Reason),
%%            {reply, true, NewState};
%%        CommandChain ->
%%            NewState = process_start_command(State, CommandChain),
%%            {reply, true, NewState}
%%    end;
%%handle_call({process_command, _CommandLine}, _From, State) ->
%%    {reply, false, State};
%%handle_call(current_state, _From, State) ->
%%    Prompt = prompt_factory:generate_prompt(State#client_handler_state.execution_state),
%%    {reply, Prompt, State};
%%handle_call({extensions, CommandLine}, _From, State) ->
%%    Generator = State#client_handler_state.extension_generator,
%%    Extensions = Generator(CommandLine),
%%    {reply, Extensions, State}.

handle_call({?PROCESS, _CommandLine}, _From, State) ->
    {reply, true, State};
handle_call(?CURRENT_STATE, _From, State) ->
    Prompt = "",
    {reply, Prompt, State};
handle_call({?EXTENSIONS, _CommandLine}, _From, State) ->
    Extensions = [],
    {reply, Extensions, State}.

handle_cast(Request, State) ->
    NewState = command_executor:process(Request, State),
    {noreply, NewState}.

%% command's normal exit
handle_info({'EXIT', _From, normal}, State) -> {noreply, State};
%% command's interrupt
handle_info({'EXIT', _From, interrupt}, State) -> {noreply, State};
%% command's abnormal exit
handle_info({'EXIT', _From, _Other}, State) -> {stop, command_exit, State};
%% other info
handle_info(_Info, State) -> {stop, enotsup, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================

%%-spec create_execution_state(GlobalConfig :: #global_config{}) ->
%%    {'ok', ExecutionState :: #execution_state{}} | {'error', Reason :: term()}.
%%create_execution_state(GlobalConfig) ->
%%    GlobalHandler = GlobalConfig#global_config.global_handler,
%%    case commands_info_helper:retrieve(GlobalHandler) of
%%        {ok, CommandsInfo} ->
%%            {ok, #execution_state{global_handler = GlobalHandler, commands_info = CommandsInfo, device_name = ?DEVICE_NAME}};
%%        {error, Reason} ->
%%            {error, Reason}
%%    end.

%%-spec process_parser_error(State ::  #client_handler_state{}, Reason :: term()) -> #client_handler_state{}.
%%process_parser_error(State, Reason) ->
%%    Error = message_helper:format(?PARSER_ERROR, [Reason]),
%%    command_helper:send_error(State, Error),
%%    command_helper:send_end(State),
%%    State.

%%-spec process_start_command(State :: #client_handler_state{}, CommandChain :: [#command_entry{}]) -> #client_handler_state{}.
%%process_start_command(State, CommandChain) ->
%%    ExecutionState = State#client_handler_state.execution_state,
%%    Request = {command_end, ExecutionState, 0},
%%    command_execution_context:process(Request, State#client_handler_state{command_chain = CommandChain}).