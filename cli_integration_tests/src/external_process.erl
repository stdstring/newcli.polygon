%% @author std-string

-module(external_process).

-define(MAX_LINE_LENGTH, 1000).

%% ====================================================================
%% API functions
%% ====================================================================

%%-export([create/1, create/2, send_data_to_process/2, receive_data_from_process/0, close/1]).
-export([create/2, send_data_to_process/2, receive_data_from_process/0, close/1]).

%%-spec create(ProcessFilename :: file:name()) -> port().
%%create(ProcessFilename) ->
%%    open_port({spawn_executable, ProcessFilename}, [{line, ?MAX_LINE_LENGTH}, stream, use_stdio, exit_status, stderr_to_stdout]).

%%-spec create(ProcessFilename :: file:name(), ProcessArgs :: [string() | binary()]) -> port().
%%create(ProcessFilename, ProcessArgs) ->
%%    open_port({spawn_executable, ProcessFilename}, [{line, ?MAX_LINE_LENGTH}, {args, ProcessArgs}, stream, use_stdio, exit_status, stderr_to_stdout]).

-spec create(Command :: string(), Dir :: string()) -> port().
create(Command, Dir) ->
    open_port({spawn, Command}, [{line, ?MAX_LINE_LENGTH}, {cd, Dir}, binary, stream, use_stdio, exit_status, stderr_to_stdout]).

-spec send_data_to_process(ProcessPort :: port(), Data :: iodata()) -> 'ok'.
send_data_to_process(ProcessPort, Data) ->
    port_command(ProcessPort, Data),
    ok.

-spec receive_data_from_process() -> [Data :: string()].
receive_data_from_process() ->
    Messages = message_reader:read_all_messages(),
    FilteredMessages = lists:filter(fun({_ProcessPort, {data, _Data}}) -> true;
                                       (_Other) ->false end, Messages),
    DataList = lists:map(fun({_ProcessPort, {data, Data}}) -> Data end, FilteredMessages),
    process_data(DataList, [], "").

-spec close(ProcessPort :: port()) -> 'ok'.
close(ProcessPort) ->
    port_close(ProcessPort),
    ok.

%% ====================================================================
%% Internal functions
%% ====================================================================

process_data([], Lines, "") -> lists:reverse(Lines);
process_data([], Lines, Line) -> lists:reverse([Line] ++ Lines);
process_data([{eol, Line} | Rest], Lines, []) ->
    process_data(Rest, [Line] ++ Lines, []);
process_data([{eol, Line} | Rest], Lines, LineStart) ->
    process_data(Rest, [LineStart ++ Line] ++ Lines, []);
process_data([{noeol, Line} | Rest], Lines, LineStart) ->
    process_data(Rest, Lines, LineStart ++ Line).