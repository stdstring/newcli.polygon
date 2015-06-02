%% @author std-string

-module(cli_terminal_endpoint).

-behaviour(gen_server).

-include("authentication_defs.hrl").
-include("client_handler_defs.hrl").
-include("common_defs.hrl").
-include("command_defs.hrl").
-include("message_defs.hrl").

-export([start/2, handle_output/2, handle_error/2, handle_end/3, stop/2]).
%% gen_server export
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(NO_RESPONSE, no_response).
-define(COMMAND_END_RESPONSE(Prompt, ExecutionState), {'end', Prompt, ExecutionState}).

%% messages
-define(DOWNTIME, "Exit due to downtime\n").

%% ====================================================================
%% API functions
%% ====================================================================

-spec start(GlobalConfig :: #global_config{}, Socket :: term()) -> {'ok', Pid :: pid()} | {'error', Reason :: term()}.
start(GlobalConfig, Socket) ->
    gen_server:start_link(?MODULE, [GlobalConfig, Socket], []).

-spec handle_output(Endpoint :: pid(), Output :: string()) -> 'ok'.
handle_output(Endpoint, Output) ->
    gen_server:cast(Endpoint, ?COMMAND_OUTPUT(Output)).

-spec handle_error(Endpoint :: pid(), Error :: string()) -> 'ok'.
handle_error(Endpoint, Error) ->
    gen_server:cast(Endpoint, ?COMMAND_ERROR(Error)).

-spec handle_end(Endpoint :: pid(), Prompt :: string(), ExecutionState :: 'ex_stop' | 'ex_continue') -> 'ok'.
handle_end(Endpoint, Prompt, ExecutionState) ->
    gen_server:cast(Endpoint, ?COMMAND_END_RESPONSE(Prompt, ExecutionState)).

-spec stop(Endpoint :: pid(), Reason :: atom()) -> 'ok'.
stop(Endpoint, Reason) ->
    gen_server:call(Endpoint, {stop, Reason}).

init([GlobalConfig, Socket]) ->
    %% TODO (std_string) : process error case
    {ok, SocketOtherSide} = inet:peername(Socket),
    case client_handler:start(GlobalConfig, self(), SocketOtherSide) of
        {ok, ClientHandler} ->
            {ok, #cli_terminal_state{socket = Socket, client_handler = ClientHandler}};
        {error, Reason} ->
            {stop, {client_handler_init, Reason}}
    end.    

handle_call({stop, Reason}, _From, State) ->
    process_notification(?EXIT_NOTIFICATION(?DOWNTIME), State),
    cleanup(State),
    {stop, {shutdown, Reason}, ok, State};
handle_call(_Request, _From, State) -> {stop, enotsup, ok, State}.

handle_cast(Request, State) ->
    process_response(Request, State),
    {noreply, State}.

handle_info({tcp, Socket, Data}, State) ->
    io:format("data = ~p~n", [Data]),
    Request = binary_to_term(Data),
    io:format("request = ~p~n", [Request]),
    Result = process_request(Request, State),
    io:format("response = ~p~n", [Result]),
    case process_response(Result, State) of
        ok ->
            inet:setopts(Socket, [{active, once}]),
            {noreply, State};
        exit ->
            cleanup(State),
            {stop, shutdown, State};
        {error, Reason} ->
            {stop, {socket_error, Reason}, State}
    end;
handle_info({tcp_closed, _Socket}, State)->
    {stop, {shutdown, socket_closed}, State};
handle_info(_Other, State) ->
    {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec process_request(Request :: term(), State :: #cli_terminal_state{}) -> Response :: term().
process_request(?COMMAND_START(CommandLine), State) ->
    ClientHandler = State#cli_terminal_state.client_handler,
    client_handler:process_command(ClientHandler, CommandLine),
    ?NO_RESPONSE;
process_request(?CURRENT_MODE_EXIT_REQUEST, State) ->
    ClientHandler = State#cli_terminal_state.client_handler,
    case client_handler:current_mode_exit(ClientHandler) of
        {true, {?EX_STOP, _Info}} -> ?CURRENT_MODE_STOP_RESPONSE;
        {true, {?EX_CONTINUE, Prompt}} -> ?CURRENT_MODE_EXIT_RESPONSE(Prompt);
        {false, Reason} -> ?ERROR(Reason)
    end;
process_request(?COMMAND_INT, State) ->
    ClientHandler = State#cli_terminal_state.client_handler,
    client_handler:interrupt_command(ClientHandler),
    ?NO_RESPONSE;
process_request(?CURRENT_STATE_REQUEST, State) ->
    ClientHandler = State#cli_terminal_state.client_handler,
    case client_handler:get_current_state(ClientHandler) of
        {true, Prompt} -> ?CURRENT_STATE_RESPONSE(Prompt);
        {false, Reason} -> ?ERROR(Reason)
    end;
process_request(?EXTENSION_REQUEST(CommandLine), State) ->
    ClientHandler = State#cli_terminal_state.client_handler,
    case client_handler:get_extensions(ClientHandler, CommandLine) of
        {true, {CommonPrefix, ExtensionList}} -> ?EXTENSION_RESPONSE(CommonPrefix, ExtensionList);
        {false, Reason} -> ?ERROR(Reason)
    end;
process_request(?EXIT, State) ->
    ClientHandler = State#cli_terminal_state.client_handler,
    client_handler:exit(ClientHandler),
    Socket = State#cli_terminal_state.socket,
    gen_tcp:close(Socket),
    ?EXIT;
process_request(?HELP_REQUEST(CommandLine), State) ->
    ClientHandler = State#cli_terminal_state.client_handler,
    case client_handler:get_help(ClientHandler, CommandLine) of
        {true, Help} -> ?HELP_RESPONSE(Help);
        {false, Reason} -> ?ERROR(Reason)
    end;
process_request(?SUITABLE_REQUEST(CommandLine), State) ->
    ClientHandler = State#cli_terminal_state.client_handler,
    case client_handler:get_suitable_commands(ClientHandler, CommandLine) of
        {true, CommandsList} -> ?SUITABLE_RESPONSE(CommandsList);
        {false, Reason} -> ?ERROR(Reason)
    end;
process_request(?LOGIN_REQUEST(Username, Password), State) ->
    ClientHandler = State#cli_terminal_state.client_handler,
    LoginResult = client_handler:login(ClientHandler, Username, Password),
    case LoginResult of
        #login_success{greeting = Greeting} -> ?LOGIN_SUCCESS_RESPONSE(Greeting);
        #login_fail{reason = Reason} -> ?LOGIN_FAIL_RESPONSE(Reason);
        #login_error{reason = Reason} -> ?LOGIN_ERROR_RESPONSE(Reason)
    end.

-spec process_response(Response :: term(), State :: #cli_terminal_state{}) ->
    'ok' | 'exit' | {'error', Reason :: atom()}.
process_response(?NO_RESPONSE, _State) -> ok;
process_response(?EXIT, _State) -> exit;
process_response(?COMMAND_OUTPUT(_Out) = Response, State) ->
    gen_tcp:send(State#cli_terminal_state.socket, term_to_binary(Response));
process_response(?COMMAND_ERROR(_Err) = Response, State) ->
    gen_tcp:send(State#cli_terminal_state.socket, term_to_binary(Response));
process_response(?COMMAND_END_RESPONSE(Prompt, ?EX_CONTINUE), State) ->
    Response = ?COMMAND_END(Prompt),
    gen_tcp:send(State#cli_terminal_state.socket, term_to_binary(Response));
process_response(?COMMAND_END_RESPONSE(_Prompt, ?EX_STOP), State) ->
    Response = ?COMMAND_STOP,
    gen_tcp:send(State#cli_terminal_state.socket, term_to_binary(Response)),
    exit;
process_response(?ERROR(_Reason) = Response, State) ->
    gen_tcp:send(State#cli_terminal_state.socket, term_to_binary(Response));
process_response(?CURRENT_STATE_RESPONSE(_Data) = Response, State) ->
    gen_tcp:send(State#cli_terminal_state.socket, term_to_binary(Response));
process_response(?EXTENSION_RESPONSE(_Prefix, _CommandsList) = Response, State) ->
    gen_tcp:send(State#cli_terminal_state.socket, term_to_binary(Response));
process_response(?HELP_RESPONSE(_Help) = Response, State) ->
    gen_tcp:send(State#cli_terminal_state.socket, term_to_binary(Response));
process_response(?SUITABLE_RESPONSE(_Data) = Response, State) ->
    gen_tcp:send(State#cli_terminal_state.socket, term_to_binary(Response));
process_response(?LOGIN_SUCCESS_RESPONSE(_Greeting) = Response, State) ->
    gen_tcp:send(State#cli_terminal_state.socket, term_to_binary(Response));
process_response(?LOGIN_FAIL_RESPONSE(_Reason) = Response, State) ->
    gen_tcp:send(State#cli_terminal_state.socket, term_to_binary(Response));
process_response(?LOGIN_ERROR_RESPONSE(_Reason) = Response, State) ->
    gen_tcp:send(State#cli_terminal_state.socket, term_to_binary(Response)),
    exit;
process_response(?CURRENT_MODE_EXIT_RESPONSE(_Prompt) = Response, State) ->
    gen_tcp:send(State#cli_terminal_state.socket, term_to_binary(Response));
process_response(?CURRENT_MODE_STOP_RESPONSE = Response, State) ->
    gen_tcp:send(State#cli_terminal_state.socket, term_to_binary(Response)),
    exit.

-spec process_notification(Notification :: term(), State :: #cli_terminal_state{}) ->
     'ok' | {'error', Reason :: atom()}.
process_notification(?EXIT_NOTIFICATION(_Message) = Notification, State) ->
    gen_tcp:send(State#cli_terminal_state.socket, term_to_binary(Notification)).

-spec cleanup(State :: #cli_terminal_state{}) -> 'ok'.
cleanup(State) ->
    gen_tcp:close(State#cli_terminal_state.socket).