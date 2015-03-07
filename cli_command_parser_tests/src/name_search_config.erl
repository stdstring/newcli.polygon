%% @author std-string

-module(name_search_config).

-export([create/0]).

-include("name_search_defs.hrl").
-include("command_defs.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

-spec create() -> name_search_table().
create() ->
    [{[{"ping", 1}], ?PING_COMMAND},
     {[{"configure", 1}, {"terminal", 1}], ?CONF_TERM_COMMAND},
     {[{"login", 4}], ?LOGIN_COMMAND},
     {[{"logout", 4}], ?LOGOUT_COMMAND},
     {[{"interface", 1}], ?INTERFACE_COMMAND},
     {[{"interface", 1}, {"range", 1}], ?IFRANGE_COMMAND},
     {[{"vlan", 1}], ?VLAN_COMMAND},
     {[{"no", 2}, {"vlan", 1}], ?NOVLAN_COMMAND},
     {[{"switchport", 2}, {"access", 1}, {"vlan", 1}], ?SWACCESS_VLAN_COMMAND},
     {[{"no", 2}, {"switchport", 1}, {"access", 1}, {"vlan", 1}], ?NOSWACCESS_VLAN_COMMAND},
     {[{"name", 2}], ?NAME_COMMAND},
     {[{"no", 2}, {"name", 1}], ?NONAME_COMMAND},
     {[{"end", 2}], ?END_COMMAND},
     {[{"exit", 2}], ?EXIT_COMMAND},
     {[{"show", 2}, {"vlan", 1}], ?SHOW_VLAN_COMMAND}].

%% ====================================================================
%% Internal functions
%% ====================================================================