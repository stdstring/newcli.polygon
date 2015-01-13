%% @author std-string

-module(command_utils).

-export([process_error/4, send_output/2, send_error/2]).

%% ====================================================================
%% API functions
%% ====================================================================

-spec process_error(Stderr :: pid(), Message :: string(), ReturnCode :: integer(), ExecContext :: [{Key :: atom(), Value :: term()}]) ->
    {ReturnValue :: integer(), ExecContext :: [{Key :: atom(), Value :: term()}]}.
process_error(Stderr, Message, ReturnCode, ExecContext) ->
    send_error(Stderr, Message),
    {ReturnCode, ExecContext}.

-spec send_output(Stdout :: pid(), Message :: string()) -> 'ok'.
send_output(Stdout, Message) ->
    gen_server:call(Stdout, {output, Message}),
    ok.

-spec send_error(Stderr :: pid(), Message :: string()) -> 'ok'.
send_error(Stderr, Message) ->
    gen_server:call(Stderr, {error, Message}),
    ok.

%% ====================================================================
%% Internal functions
%% ====================================================================