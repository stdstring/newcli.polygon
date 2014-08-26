%% @author std-string

-module(cli_terminal_endpoint).

-behaviour(gen_server).

-include("common_defs.hrl").
-include("cli_terminal_message_defs.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

-export([start/2, handle_output/2, handle_error/2, handle_end/2]).
%% gen_server export
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-spec start(GlobalConfig :: #global_config{}, Socket :: term()) -> {'ok', Pid :: pid()} | {'error', Reason :: term()}.
start(GlobalConfig, Socket) ->
    io:format("cli_terminal_endpoint:start/1 ~n", []),
    gen_server:start_link(?MODULE, [GlobalConfig, Socket], []).

-spec handle_output(Endpoint :: pid(), Output :: string()) -> 'ok'.
handle_output(Endpoint, Output) ->
    gen_server:cast(Endpoint, #command_out{data = Output}).

-spec handle_error(Endpoint :: pid(), Error :: string()) -> 'ok'.
handle_error(Endpoint, Error) ->
    gen_server:cast(Endpoint, #command_err{data = Error}).

-spec handle_end(Endpoint :: pid(), Prompt :: string()) -> 'ok'.
handle_end(Endpoint, Prompt) ->
    gen_server:cast(Endpoint, #'end'{prompt = Prompt}).

init([GlobalConfig, Socket]) ->
    io:format("cli_terminal_endpoint:init/1 ~n", []),
    case client_handler:start(GlobalConfig, self()) of
        {ok, ClientHandler} ->
            io:format("cli_terminal_endpoint:init/1, create client_handler instance ~n", []),
            {ok, #cli_terminal_state{socket = Socket, client_handler = ClientHandler}};
        {error, Reason} ->
            io:format("cli_terminal_endpoint:init/1, create client_handler failed: ~p~n", [Reason]),
            {stop, {client_handler_init, Reason}}
    end.    

handle_call(_Request, _From, State) -> {stop, enotsup, State}.

handle_cast(Request, State) ->
    process_response(Request, State),
    {noreply, State}.

handle_info({tcp, Socket, Data}, State) ->
    io:format("Request binary: ~p~n", [Data]),
    Request = binary_to_term(Data),
    io:format("Request: ~p~n", [Request]),
    Result = process_request(Request, Socket),
    case process_response(Result, Socket) of
        ok ->
            inet:setopts(Socket, [{active, once}]),
            {noreply, State};
        {error, Reason} ->
            {stop, {socket_error, Reason}, State}
    end;
handle_info({tcp_closed, _Socket}, State)->
    {stop, {shutdown, socket_closed}, State};
handle_info(Other, State) ->
    io:format("Other: ~p~n", [Other]),
    {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec process_request(Request :: term(), State :: #cli_terminal_state{}) -> Response :: term().
process_request(#command{command_line = CommandLine}, State) ->
    ClientHandler = State#cli_terminal_state.client_handler,
    case client_handler:process_command(ClientHandler, CommandLine) of
        true -> no_response;
        false -> #error{reason = "There is running the other command, now\n"}
    end;
process_request(#interrupt{}, State) ->
    ClientHandler = State#cli_terminal_state.client_handler,
    client_handler:interrupt_command(ClientHandler),
    no_response;
process_request(#current_state_request{}, State) ->
    ClientHandler = State#cli_terminal_state.client_handler,
    Prompt = client_handler:get_current_state(ClientHandler),
    process_response(#current_state_response{prompt = Prompt}, State);
process_request(#extension_request{command_line = CommandLine}, State) ->
    ClientHandler = State#cli_terminal_state.client_handler,
    ExtensionList = client_handler:get_extensions(ClientHandler, CommandLine),
    process_response(#extension_response{extension_list = ExtensionList}, State);
process_request(#exit{}, _State) ->
    %% some action
    no_response.

-spec process_response(Response :: term(), State :: #cli_terminal_state{}) -> 'ok' | {'error', Reason :: atom()}.
process_response(no_response, _State) -> ok;
process_response(#command_out{} = Response, State) ->
    gen_tcp:send(State#cli_terminal_state.socket, term_to_binary(Response));
process_response(#command_err{} = Response, State) ->
    gen_tcp:send(State#cli_terminal_state.socket, term_to_binary(Response));
process_response(#'end'{} = Response, State) ->
    gen_tcp:send(State#cli_terminal_state.socket, term_to_binary(Response));
process_response(#error{} = Response, State) ->
    gen_tcp:send(State#cli_terminal_state.socket, term_to_binary(Response));
process_response(#current_state_response{} = Response, State) ->
    gen_tcp:send(State#cli_terminal_state.socket, term_to_binary(Response));
process_response(#extension_response{} = Response, State) ->
    gen_tcp:send(State#cli_terminal_state.socket, term_to_binary(Response)).