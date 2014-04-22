%% @author std-string

-module(erlang_term_utils).

%% ====================================================================
%% API functions
%% ====================================================================
-export([read_from_file/1, write_to_file/2, append_to_file/2]).

read_from_file(Filename) ->
    read_from_file_impl(file:open(Filename, [read])).

write_to_file(Filename, Term) ->
    write_to_file_impl(file:open(Filename, [write]), Term).

append_to_file(Filename, Term) ->
    write_to_file_impl(file:open(Filename, [append]), Term).

%% ====================================================================
%% Internal functions
%% ====================================================================

read_from_file_impl({error, Reason}) -> error({file_read_error, Reason});
read_from_file_impl({ok, IoDevice}) ->
    try io:read(IoDevice, "") of
        {ok, Term} -> Term;
        eof -> error({file_read_error, eof});
        {error, Reason} -> error({file_read_error, Reason})
    after
        file:close(IoDevice)
    end.

write_to_file_impl({error, Reason}, _Term) -> error({file_write_error, Reason});
write_to_file_impl({ok, IoDevice}, Term) ->
    try io:write(IoDevice, Term)
    after
        file:close(IoDevice)
    end.