%% @author std-string

-module(output_endpoint).

-behaviour(gen_server).

-include("output_endpoint_defs.hrl").
-include("message_defs.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

-export([start/1, send_to_client/3]).
%% gen_server export
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start(OutputPid) -> start_service(OutputPid).

send_to_client(EndpointPid, CompletionCode, CliMode) ->
    gen_server:call(EndpointPid, {CompletionCode, CliMode}).

init(OutputPid) ->
    State = #output_endpoint_state{output_pid = OutputPid},
    {ok, State}.

handle_call({CompletionCode, CliMode}, _From, State) ->
    OutputPid = State#output_endpoint_state.output_pid,
    OutputBuffer = lists:reverse(State#output_endpoint_state.output_buffer),
    lists:foreach(fun(Message) -> OutputPid ! Message end, OutputBuffer),
    OutputPid ! #command_end{completion_code = CompletionCode, cli_mode = CliMode},
    {reply, output_complete, State#output_endpoint_state{output_buffer = []}}.

handle_cast(#command_output{} = CommandOutput, State) ->
    {noreply, handle_command_message(CommandOutput, State)};
handle_cast(#command_error{} = CommandError, State) ->
    {noreply, handle_command_message(CommandError, State)}.

handle_info(_Info, _State) -> error(not_supported).

terminate(_Reason, _State) -> true.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================

start_service(OutputPid) ->
    case gen_server:start_link(?MODULE, OutputPid, []) of
        {ok, Pid} -> Pid;
        {error, Error} -> {output_endpoint, Error}
    end.

handle_command_message(CommandMessage, State) ->
    OutputBuffer = State#output_endpoint_state.output_buffer,
    NewOutputBuffer = [CommandMessage] ++ OutputBuffer,
    State#output_endpoint_state{output_buffer = NewOutputBuffer}.
