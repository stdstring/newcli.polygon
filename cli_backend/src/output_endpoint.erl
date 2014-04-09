%% @author std-string

-module(output_endpoint).

-behaviour(gen_server).

-include("output_endpoint_defs.hrl").
-include("message_defs.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

-export([start/1, send_result/3, send_fail/2]).
%% gen_server export
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-spec start(OutputPid :: pid()) -> {'ok', Pid :: pid()} | {'error', Reason :: term()}.
start(OutputPid) -> gen_server:start_link(?MODULE, OutputPid, []).

-spec send_result(EndpointPid :: pid(), CompletionCode :: integer(), CliMode :: string()) -> 'output_complete'.
send_result(EndpointPid, CompletionCode, CliMode) ->
    gen_server:call(EndpointPid, #command_end{completion_code = CompletionCode, cli_mode = CliMode}).

-spec send_fail(EndpointPid :: pid(), Reason::term()) -> 'output_complete'.
send_fail(EndpointPid, Reason) ->
    gen_server:call(EndpointPid, #command_fail{reason = Reason}).

init(OutputPid) ->
    State = #output_endpoint_state{output_pid = OutputPid},
    {ok, State}.

handle_call(#command_end{} = FinalMessage, _From, State) ->
    send_to_client(FinalMessage, State),
    {reply, output_complete, State#output_endpoint_state{output_buffer = []}};
handle_call(#command_fail{} = FinalMessage, _From, State) ->
    send_to_client(FinalMessage, State),
    {reply, output_complete, State#output_endpoint_state{output_buffer = []}}.

handle_cast(#command_output{} = CommandOutput, State) ->
    {noreply, handle_command_message(CommandOutput, State)};
handle_cast(#command_error{} = CommandError, State) ->
    {noreply, handle_command_message(CommandError, State)}.

handle_info(_Info, State) -> {stop, enotsup, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec send_to_client(FinalMessage :: #command_end{} | #command_fail{}, State :: #output_endpoint_state{}) -> 'ok'.
send_to_client(FinalMessage, State) ->
    OutputPid = State#output_endpoint_state.output_pid,
    OutputBuffer = lists:reverse(State#output_endpoint_state.output_buffer),
    lists:foreach(fun(Message) -> gen_server:cast(OutputPid, Message) end, OutputBuffer),
    gen_server:cast(OutputPid, FinalMessage),
    ok.

-spec handle_command_message(CommandMessage :: #command_output{} | #command_error{}, State :: #output_endpoint_state{}) -> #output_endpoint_state{}.
handle_command_message(CommandMessage, State) ->
    OutputBuffer = State#output_endpoint_state.output_buffer,
    NewOutputBuffer = [CommandMessage] ++ OutputBuffer,
    State#output_endpoint_state{output_buffer = NewOutputBuffer}.
