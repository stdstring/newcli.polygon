%% @author std-string

-module(external_process).

-define(MAX_LINE_LENGTH, 1000).

%% ====================================================================
%% API functions
%% ====================================================================

-export([create/1, create/2, send_data_to_process/2, receive_data_from_process/1, close/1]).

-spec create(ProcessFilename :: file:name()) -> port().
create(ProcessFilename) ->
    open_port({spawn_executable, ProcessFilename}, [{line, ?MAX_LINE_LENGTH}]).

-spec create(ProcessFilename :: file:name(), ProcessArgs :: [string() | binary()]) -> port().
create(ProcessFilename, ProcessArgs) ->
    open_port({spawn_executable, ProcessFilename}, [{line, ?MAX_LINE_LENGTH}, {args, ProcessArgs}]).

-spec send_data_to_process(ProcessPort :: port(), Data :: iodata()) -> 'ok'.
send_data_to_process(ProcessPort, Data) ->
    port_command(ProcessPort, Data),
    ok.

-spec receive_data_from_process(ProcessPort :: port()) -> [Data :: string()].
receive_data_from_process(ProcessPort) ->
    Messages = message_reader:read_all_messages(),
    FilteredMessages = lists:filter(fun({ProcessPort, {data, _Data}}) -> true;
                                       (_Other) ->false end, Messages),
    ok.

close(_ProcessPort) -> ok.

%% ====================================================================
%% Internal functions
%% ====================================================================
