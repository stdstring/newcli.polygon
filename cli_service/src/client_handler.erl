%% @author std-string

-module(client_handler).

-behaviour(gen_server).

-include("authentication_defs.hrl").
-include("client_handler_defs.hrl").
-include("command_defs.hrl").
-include("common_defs.hrl").

-define(COMMAND_CREATION_ERROR, "Command's creation is failed due to the following reason: ~w\n").
-define(COMMAND_ALREADY_RUN, "There is running the other command, now\n").

-export([start/3,
         process_command/2,
         interrupt_command/1,
         get_current_state/1,
         get_extensions/2,
         exit/1,
         send_output/2,
         send_error/2,
         finish_command/3,
         finish_exec/3,
         get_help/2,
         get_suitable_commands/2]).
%% gen_server export
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================

-spec start(GlobalConfig :: #global_config{}, Endpoint :: pid(), SocketOtherSide :: {Address :: tuple(), Port :: pos_integer()}) ->
    {'ok', Pid :: pid()} | {'error', Reason :: term()}.
start(GlobalConfig, Endpoint, SocketOtherSide) ->
    gen_server:start_link(?MODULE, [GlobalConfig, Endpoint, SocketOtherSide], []).

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

-spec get_help(Handler :: pid(), CommandLine :: string()) -> string().
get_help(Handler, CommandLine) ->
    gen_server:call(Handler, {?HELP, CommandLine}).

-spec get_suitable_commands(Handler :: pid(), CommandLine :: string()) -> [string()].
get_suitable_commands(Handler, CommandLine) ->
    gen_server:call(Handler, {?SUITABLE_COMMANDS, CommandLine}).

init([GlobalConfig, Endpoint, SocketOtherSide]) ->
    %% for catching exit signals from commands
    process_flag(trap_exit, true),
    CommandModule = module_name_generator:generate(?ENTRY_MODULE_PREFIX, SocketOtherSide),
    case cli_fsm:start(GlobalConfig#global_config.cli_fsm) of
        {ok, CliFsm} ->
            State = #client_handler_state{config = GlobalConfig,
                                          endpoint = Endpoint,
                                          command_module = CommandModule,
                                          cli_fsm =CliFsm},
            {ok, State};
        {error, Reason} ->
            {stop, Reason}
    end.

handle_call({?PROCESS, CommandLine}, _From, #client_handler_state{current_command = undefined} = State) ->
    GlobalConfig = State#client_handler_state.config,
    %% TODO (std_string) : think about caching
    LexConfig = lex_analyzer_config:create(true),
    NameConfig = name_search_config:create(GlobalConfig#global_config.commands),
    SyntaxConfig = syntax_analyzer_config:create(NameConfig),
    CommandModule = State#client_handler_state.command_module,
    case command_factory:process(CommandLine, LexConfig, SyntaxConfig, CommandModule) of
        {true, CommandFun} ->
            Command = process_start_command(State, CommandFun),
            {reply, true, State#client_handler_state{current_command = Command}};
        {false, Reason} ->
            process_command_creation_error(State, Reason),
            {reply, false, State}
    end;
handle_call({?PROCESS, _CommandLine}, _From, State) ->
    command_helper:send_error(State, ?COMMAND_ALREADY_RUN),
    {reply, false, State};
handle_call(?CURRENT_STATE, _From, State) ->
    Prompt = prompt_factory:generate_prompt(State),
    {reply, Prompt, State};
handle_call({?EXTENSIONS, CommandLine}, _From, State) ->
    Extensions = client_handler_helper:get_extensions(CommandLine, State),
    {reply, Extensions, State};
handle_call({?HELP, CommandLine}, _From, State) ->
    Help = client_handler_helper:get_help(CommandLine, State),
    {reply, Help, State};
handle_call({?SUITABLE_COMMANDS, CommandLine}, _From, State) ->
    Commands = client_handler_helper:get_suitable_commands(CommandLine, State),
    {reply, Commands, State}.

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

-spec process_start_command(State :: #client_handler_state{},
                            CommandFun :: fun((CliFsm :: pid(), ClientHandler :: pid(), Context :: [{atom(), term()}]) -> 'ok')) ->
    pid().
process_start_command(State, CommandFun) ->
    ExecutionContext = create_exec_context(State),
    CliFsm = State#client_handler_state.cli_fsm,
    ClientHandler = self(),
    spawn_link(fun() -> CommandFun(CliFsm, ClientHandler, ExecutionContext) end).

-spec process_command_creation_error(State ::  #client_handler_state{}, Reason :: term()) -> 'ok'.
process_command_creation_error(State, Reason) ->
    Error = string_utils:format(?COMMAND_CREATION_ERROR, [Reason]),
    command_helper:send_error(State, Error),
    command_helper:send_end(State),
    ok.

-spec create_exec_context(State :: #client_handler_state{}) -> [{Key :: atom(), Balue :: term()}].
create_exec_context(#client_handler_state{user = undefined}) ->
    [];
create_exec_context(#client_handler_state{user = User}) ->
    [{?USER_KEY, User}].