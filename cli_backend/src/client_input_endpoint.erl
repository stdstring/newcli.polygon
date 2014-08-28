%% @author std-string

-module(client_input_endpoint).

-behaviour(gen_server).

-include("message_defs.hrl").
-include("common_defs.hrl").

-record(client_state, {global_config = #global_config{} :: #global_config{}, client_config = #client_config{} :: #client_config{}}).

%% ====================================================================
%% API functions
%% ====================================================================

-export([start/3, process_command/2]).
%% gen_server export
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-spec start(GlobalConfig :: #global_config{}, User :: #user{}, ClientOutput :: pid()) -> pid() | {'client_input_endpoint', Error :: term()}.
start(GlobalConfig, User, ClientOutput) ->
    gen_server:start_link(?MODULE, {GlobalConfig, User, ClientOutput}, []).

-spec process_command(InputEndpoint :: pid(), Command :: string()) -> Result :: atom().
process_command(InputEndpoint, CommandLine) ->
    gen_server:call(InputEndpoint, CommandLine).

init({GlobalConfig, User, ClientOutput}) ->
    case cli_fsm:start(GlobalConfig) of
        {cli_fsm, Error} -> {stop, {cli_fsm, Error}};
        {ok, Pid} ->
            ClientConfig = #client_config{user = User, cli_fsm = Pid, output = ClientOutput},
            {ok, #client_state{global_config = GlobalConfig, client_config = ClientConfig}}
    end.

handle_call(#command{message = CommandLine}, {From, _Tag}, State) ->
    GlobalConfig = State#client_state.global_config,
    ClientConfig = State#client_state.client_config,
    NewClientConfig = ClientConfig#client_config{output = From},
    case command_execution_context:execute(CommandLine, GlobalConfig, NewClientConfig) of
        false -> {stop, {shutdown, session_terminated}, session_terminated, State#client_state{client_config = NewClientConfig}};
        true -> {reply, command_processed, State#client_state{client_config = NewClientConfig}}
    end.

handle_cast(_Request, State) -> {stop, enotsup, State}.

handle_info(_Info, State) -> {stop, enotsup, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================
