%% @author stdstring

-module(string_data_utils).

%% ====================================================================
%% API functions
%% ====================================================================

-export([add_trailing_line_feed/1, remove_trailing_line_feed/1, remove_all_trailing_line_feeds/1]).

-spec add_trailing_line_feed(Source :: string()) -> string().
add_trailing_line_feed(Source) ->
    add_trailing_line_feed_impl(lists:reverse(Source)).

-spec remove_trailing_line_feed(Source :: string()) -> string().
remove_trailing_line_feed(Source) ->
    remove_trailing_line_feed_impl(lists:reverse(Source)).

-spec remove_all_trailing_line_feeds(Source :: string()) -> string().
remove_all_trailing_line_feeds(Source) ->
    string:strip(Source, right, $\n).

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec add_trailing_line_feed_impl(Source :: string()) -> string().
add_trailing_line_feed_impl([$\n | _Rest] = Source) ->
    lists:reverse(Source);
add_trailing_line_feed_impl(Source) ->
    lists:reverse("\n" ++ Source).

-spec remove_trailing_line_feed_impl(Source :: string()) -> string().
remove_trailing_line_feed_impl([$\n | Rest]) ->
    lists:reverse(Rest);
remove_trailing_line_feed_impl(Source) ->
    lists:reverse(Source).