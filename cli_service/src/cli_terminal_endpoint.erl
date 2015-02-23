%% @author std-string

-module(cli_terminal_endpoint).

-behaviour(gen_server).

-include("authentication_defs.hrl").
-include("common_defs.hrl").

-export([start/2, handle_output/2, handle_error/2, handle_end/2]).
%% gen_server export
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(COMMAND_START(CommandLine), {command, CommandLine}).
-define(COMMAND_OUTPUT(Output), {command_out, Output}).
-define(COMMAND_ERROR(Error), {command_err, Error}).
-define(COMMAND_END(Prompt), {'end', Prompt}).
-define(COMMAND_INT, {interrupt}).
-define(CURRENT_MODE_EXIT, {current_mode_exit}).
-define(CURRENT_STATE_REQUEST, {current_state_request}).
-define(CURRENT_STATE_RESPONSE(Prompt), {current_state_response, Prompt}).
-define(EXTENSION_REQUEST(CommandLine), {extension_request, CommandLine}).
-define(EXTENSION_RESPONSE(ExtensionList), {extension_response, ExtensionList}).
-define(EXIT, {exit}).
-define(ERROR, {error}).
-define(HELP_REQUEST(CommandLine), {help_request, CommandLine}).
-define(HELP_RESPONSE(Help), {help_response, Help}).
-define(SUITABLE_REQUEST(CommandLine), {suitable_commands_request, CommandLine}).
-define(SUITABLE_RESPONSE(CommandList), {suitable_commands_response, CommandList}).
-define(NO_RESPONSE, no_response).

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

-spec handle_end(Endpoint :: pid(), Prompt :: string()) -> 'ok'.
handle_end(Endpoint, Prompt) ->
    gen_server:cast(Endpoint, ?COMMAND_END(Prompt)).

init([GlobalConfig, Socket]) ->
    %% TODO (std_string) : process error case
    {ok, SocketOtherSide} = inet:peername(Socket),
    case client_handler:start(GlobalConfig, self(), SocketOtherSide) of
        {ok, ClientHandler} ->
            {ok, #cli_terminal_state{socket = Socket, client_handler = ClientHandler}};
        {error, Reason} ->
            {stop, {client_handler_init, Reason}}
    end.    

handle_call(_Request, _From, State) -> {stop, enotsup, State}.

handle_cast(Request, State) ->
    process_response(Request, State),
    {noreply, State}.

handle_info({tcp, Socket, Data}, State) ->
    Request = binary_to_term(Data),
    Result = process_request(Request, State),
    case process_response(Result, State) of
        ok ->
            inet:setopts(Socket, [{active, once}]),
            {noreply, State};
        {error, Reason} ->
            {stop, {socket_error, Reason}, State}
    end;
handle_info({tcp_closed, _Socket}, State)->
    {stop, {shutdown, socket_closed}, State};
handle_info(_Other, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec process_request(Request :: term(), State :: #cli_terminal_state{}) -> Response :: term().
process_request(?COMMAND_START(CommandLine), State) ->
    ClientHandler = State#cli_terminal_state.client_handler,
    client_handler:process_command(ClientHandler, CommandLine),
    ?NO_RESPONSE;
process_request(?CURRENT_MODE_EXIT, _State) ->
    %%ClientHandler = State#cli_terminal_state.client_handler,
    %%client_handler:process_command(ClientHandler, CommandLine),
    ?NO_RESPONSE;
process_request(?COMMAND_INT, State) ->
    ClientHandler = State#cli_terminal_state.client_handler,
    client_handler:interrupt_command(ClientHandler),
    ?NO_RESPONSE;
process_request(?CURRENT_STATE_REQUEST, State) ->
    ClientHandler = State#cli_terminal_state.client_handler,
    Prompt = client_handler:get_current_state(ClientHandler),
    ?CURRENT_STATE_RESPONSE(Prompt);
process_request(?EXTENSION_REQUEST(CommandLine), State) ->
    ClientHandler = State#cli_terminal_state.client_handler,
    ExtensionList = client_handler:get_extensions(ClientHandler, CommandLine),
    ?EXTENSION_RESPONSE(ExtensionList);
process_request(?EXIT, _State) ->
    %% some action
    ?NO_RESPONSE;
process_request(?HELP_REQUEST(CommandLine), State) ->
    ClientHandler = State#cli_terminal_state.client_handler,
    Help = client_handler:get_help(ClientHandler, CommandLine),
    ?HELP_RESPONSE(Help);
process_request(?SUITABLE_REQUEST(CommandLine), State) ->
    ClientHandler = State#cli_terminal_state.client_handler,
    CommandList = client_handler:get_suitable_commands(ClientHandler, CommandLine),
    ?SUITABLE_RESPONSE(CommandList).

-spec process_response(Response :: term(), State :: #cli_terminal_state{}) -> 'ok' | {'error', Reason :: atom()}.
process_response(?NO_RESPONSE, _State) -> ok;
process_response(?COMMAND_OUTPUT(_Out) = Response, State) ->
    gen_tcp:send(State#cli_terminal_state.socket, term_to_binary(Response));
process_response(?COMMAND_ERROR(_Err) = Response, State) ->
    gen_tcp:send(State#cli_terminal_state.socket, term_to_binary(Response));
process_response(?COMMAND_END(_Prompt) = Response, State) ->
    gen_tcp:send(State#cli_terminal_state.socket, term_to_binary(Response));
process_response(?ERROR = Response, State) ->
    gen_tcp:send(State#cli_terminal_state.socket, term_to_binary(Response));
process_response(?CURRENT_STATE_RESPONSE(_Data) = Response, State) ->
    gen_tcp:send(State#cli_terminal_state.socket, term_to_binary(Response));
process_response(?EXTENSION_RESPONSE(_Data) = Response, State) ->
    gen_tcp:send(State#cli_terminal_state.socket, term_to_binary(Response));
process_response(?HELP_RESPONSE(_Help) = Response, State) ->
    gen_tcp:send(State#cli_terminal_state.socket, term_to_binary(Response));
process_response(?SUITABLE_RESPONSE(_Data) = Response, State) ->
    gen_tcp:send(State#cli_terminal_state.socket, term_to_binary(Response)).