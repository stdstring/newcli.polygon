%% @author std-string
%% @doc @todo Add description to erlang_term_utils.


-module(erlang_term_utils).

%% ====================================================================
%% API functions
%% ====================================================================
-export([read_from_file/1]).

read_from_file(Filename) ->
    read_from_file_impl(file:open(Filename, [read])).

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