%% @author std-string

-module(name_search_config).

-export([create/1]).

-include("name_search_defs.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

-spec create(Commands :: [{CommandName :: atom(), CommandModule :: atom()}]) -> name_search_table().
create(Commands) ->
    name_search_factory:create(lists:map(fun({Name, Module}) -> {Name, Module:get_command_body()} end, Commands)).