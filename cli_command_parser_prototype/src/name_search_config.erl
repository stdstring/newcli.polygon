-module(name_search_config).

-export([create_config/0]).

-define(MODULE_NAME, commands_example).

-define(PING_FUNCTION, ping_exec).
-define(CONF_TERM_FUNCTION, configure_terminal_exec).
-define(LOGIN_FUNCTION, login_exec).
-define(LOGOUT_FUNCTION, logout_exec).
-define(INTERFACE_FUNCTION, interface_exec).
-define(IFRANGE_FUNCTION, interface_range_exec).
-define(VLAN_FUNCTION, vlan_exec).
-define(NOVLAN_FUNCTION, novlan_exec).
-define(SWACCESS_VLAN_FUNCTION, swaccess_vlan_exec).
-define(NOSWACCESS_VLAN_FUNCTION, noswaccess_vlan_exec).
-define(NAME_FUNCTION, name_exec).
-define(NONAME_FUNCTION, noname_exec).
-define(END_FUNCTION, end_exec).
-define(EXIT_FUNCTION, exit_exec).
-define(SHOW_VLAN_FUNCTION, show_vlan_exec).

%% ====================================================================
%% API functions
%% ====================================================================

create_config() ->
    [{[{"ping", 1}], ?MODULE_NAME, ?PING_FUNCTION},
     {[{"configure", 1}, {"terminal", 1}], ?MODULE_NAME, ?CONF_TERM_FUNCTION},
     {[{"login", 4}], ?MODULE_NAME, ?LOGIN_FUNCTION},
     {[{"logout", 4}], ?MODULE_NAME, ?LOGOUT_FUNCTION},
     {[{"interface", 1}], ?MODULE_NAME, ?INTERFACE_FUNCTION},
     {[{"interface", 1}, {"range", 1}], ?MODULE_NAME, ?IFRANGE_FUNCTION},
     {[{"vlan", 1}], ?MODULE_NAME, ?VLAN_FUNCTION},
     {[{"no", 2}, {"vlan", 1}], ?MODULE_NAME, ?NOVLAN_FUNCTION},
     {[{"switchport", 2}, {"access", 1}, {"vlan", 1}], ?MODULE_NAME, ?SWACCESS_VLAN_FUNCTION},
     {[{"no", 2}, {"switchport", 1}, {"access", 1}, {"vlan", 1}], ?MODULE_NAME, ?NOSWACCESS_VLAN_FUNCTION},
     {[{"name", 2}], ?MODULE_NAME, ?NAME_FUNCTION},
     {[{"no", 2}, {"name", 1}], ?MODULE_NAME, ?NONAME_FUNCTION},
     {[{"end", 2}], ?MODULE_NAME, ?END_FUNCTION},
     {[{"exit", 2}], ?MODULE_NAME, ?EXIT_FUNCTION},
     {[{"show", 2}, {"vlan", 1}], ?MODULE_NAME, ?SHOW_VLAN_FUNCTION}].

%% ====================================================================
%% Internal functions
%% ====================================================================