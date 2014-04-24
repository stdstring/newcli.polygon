%% @author stdstring

-module(string_data_utils).

%% ====================================================================
%% API functions
%% ====================================================================

-export([add_trailing_line_feed/1, remove_trailing_line_feed/1]).

-spec add_trailing_line_feed(Source :: string()) -> string().
add_trailing_line_feed(Source) ->
    remove_trailing_line_feed(Source) + $\n.

-spec remove_trailing_line_feed(Source :: string()) -> string().
remove_trailing_line_feed(Source) ->
    string:strip(Source, right, $\n).

%% ====================================================================
%% Internal functions
%% ====================================================================

