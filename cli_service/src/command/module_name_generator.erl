%% @author stdstring

-module(module_name_generator).

-export([generate/1]).

%% ====================================================================
%% API functions
%% ====================================================================

-spec generate(Prefix :: atom()) -> atom().
generate(Prefix) ->
    list_to_atom(atom_to_list(Prefix) ++ generate_unique_suffix()).

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec generate_unique_suffix() -> string().
generate_unique_suffix() ->
    "_test".