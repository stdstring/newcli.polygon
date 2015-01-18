%% @author std-string

-module(cli_terminal_endpoint).

-behaviour(gen_server).

-include("authentication_defs.hrl").
-include("common_defs.hrl").

-export([start/2, handle_output/2, handle_error/2, handle_end/2]).
%% gen_server export
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================

-spec start(GlobalConfig :: #global_config{}, Socket :: term()) -> {'ok', Pid :: pid()} | {'error', Reason :: term()}.
start(GlobalConfig, Socket) ->
    io:format("cli_terminal_endpoint:start~n", []),
    %%io:format("Socket other side:~p~n", [inet:peername(Socket)]),
    gen_server:start_link(?MODULE, [GlobalConfig, Socket], []).

-spec handle_output(Endpoint :: pid(), Output :: string()) -> 'ok'.
handle_output(Endpoint, Output) ->
    io:format("cli_terminal_endpoint:handle_output Output=~p~n", [Output]),
    gen_server:cast(Endpoint, {command_out, Output}).

-spec handle_error(Endpoint :: pid(), Error :: string()) -> 'ok'.
handle_error(Endpoint, Error) ->
    io:format("cli_terminal_endpoint:handle_error Error=~p~n", [Error]),
    gen_server:cast(Endpoint, {command_err, Error}).

-spec handle_end(Endpoint :: pid(), Prompt :: string()) -> 'ok'.
handle_end(Endpoint, Prompt) ->
    io:format("cli_terminal_endpoint:handle_end Prompt=~p~n", [Prompt]),
    gen_server:cast(Endpoint, {'end', Prompt}).

init([GlobalConfig, Socket]) ->
    io:format("cli_terminal_endpoint:init ~n", []),
    %% TODO (std_string) : process error case
    {ok, SocketOtherSide} = inet:peername(Socket),
    case client_handler:start(GlobalConfig, self(), SocketOtherSide) of
        {ok, ClientHandler} ->
            io:format("client_handler:start success ~n", []),
            {ok, #cli_terminal_state{socket = Socket, client_handler = ClientHandler}};
        {error, Reason} ->
            io:format("client_handler:start error=~p~n", [Reason]),
            {stop, {client_handler_init, Reason}}
    end.    

handle_call(_Request, _From, State) -> {stop, enotsup, State}.

handle_cast(Request, State) ->
    process_response(Request, State),
    {noreply, State}.

handle_info({tcp, Socket, Data}, State) ->
    io:format("cli_terminal_endpoint:handle_info Data=~p~n", [Data]),
    Request = binary_to_term(Data),
    io:format("cli_terminal_endpoint:handle_info Request=~p~n", [Request]),
    Result = process_request(Request, State),
    io:format("cli_terminal_endpoint:handle_info Result=~p~n", [Result]),
    case process_response(Result, State) of
        ok ->
            inet:setopts(Socket, [{active, once}]),
            {noreply, State};
        {error, Reason} ->
            io:format("cli_terminal_endpoint:handle_info process_response error=~p~n", [Reason]),
            {stop, {socket_error, Reason}, State}
    end;
handle_info({tcp_closed, _Socket}, State)->
    {stop, {shutdown, socket_closed}, State};
handle_info(_Other, State) ->
    {noreply, State}.

terminate(Reason, _State) ->
    io:format("cli_terminal_endpoint:terminate due to ~p~n", [Reason]),
    ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec process_request(Request :: term(), State :: #cli_terminal_state{}) -> Response :: term().
process_request({command, CommandLine}, State) ->
    ClientHandler = State#cli_terminal_state.client_handler,
    %%case client_handler:process_command(ClientHandler, CommandLine) of
    %%    true -> no_response;
    %%    false -> {error, "There is running the other command, now\n"}
    %%end;
    client_handler:process_command(ClientHandler, CommandLine),
    no_response;
process_request({interrupt}, State) ->
    ClientHandler = State#cli_terminal_state.client_handler,
    client_handler:interrupt_command(ClientHandler),
    no_response;
process_request({current_state_request}, State) ->
    ClientHandler = State#cli_terminal_state.client_handler,
    Prompt = client_handler:get_current_state(ClientHandler),
    {current_state_response, Prompt};
process_request({extension_request, CommandLine}, State) ->
    ClientHandler = State#cli_terminal_state.client_handler,
    ExtensionList = client_handler:get_extensions(ClientHandler, CommandLine),
    {extension_response, ExtensionList};
process_request({exit}, _State) ->
    %% some action
    no_response.

-spec process_response(Response :: term(), State :: #cli_terminal_state{}) -> 'ok' | {'error', Reason :: atom()}.
process_response(no_response, _State) -> ok;
process_response({command_out, _Out} = Response, State) ->
    gen_tcp:send(State#cli_terminal_state.socket, term_to_binary(Response));
process_response({command_err, _Err} = Response, State) ->
    gen_tcp:send(State#cli_terminal_state.socket, term_to_binary(Response));
process_response({'end', _Prompt} = Response, State) ->
    gen_tcp:send(State#cli_terminal_state.socket, term_to_binary(Response));
process_response({error} = Response, State) ->
    gen_tcp:send(State#cli_terminal_state.socket, term_to_binary(Response));
process_response({current_state_response, _Data} = Response, State) ->
    gen_tcp:send(State#cli_terminal_state.socket, term_to_binary(Response));
process_response({extension_response, _Data} = Response, State) ->
    gen_tcp:send(State#cli_terminal_state.socket, term_to_binary(Response)).