%% @author std-string

-module(name_search_config).

-export([create/1]).

-include("name_search_defs.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

%%-spec create() -> name_search_table().
%%create() ->
%%    [{[{"ping", 1}], ?PING_MODULE},
%%     {[{"configure", 1}, {"terminal", 1}], ?CONF_TERM_MODULE},
%%     {[{"login", 4}], ?LOGIN_MODULE},
%%     {[{"logout", 4}], ?LOGOUT_MODULE},
%%     {[{"interface", 1}], ?INTERFACE_MODULE},
%%     {[{"interface", 1}, {"range", 1}], ?IFRANGE_MODULE},
%%     {[{"vlan", 1}], ?VLAN_MODULE},
%%     {[{"no", 2}, {"vlan", 1}], ?NOVLAN_MODULE},
%%     {[{"switchport", 2}, {"access", 1}, {"vlan", 1}], ?SWACCESS_VLAN_MODULE},
%%     {[{"no", 2}, {"switchport", 1}, {"access", 1}, {"vlan", 1}], ?NOSWACCESS_VLAN_MODULE},
%%     {[{"name", 2}], ?NAME_MODULE},
%%     {[{"no", 2}, {"name", 1}], ?NONAME_MODULE},
%%     {[{"end", 2}], ?END_MODULE},
%%     {[{"exit", 2}], ?EXIT_MODULE},
%%     {[{"show", 2}, {"vlan", 1}], ?SHOW_VLAN_MODULE}].

-spec create(Commands :: [{CommandName :: atom(), CommandModule :: atom()}]) -> name_search_table().
create(_Commands) ->
    [].

%% ====================================================================
%% Internal functions
%% ====================================================================