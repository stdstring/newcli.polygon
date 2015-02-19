%% module definitions

-define(PING_MODULE, ping_module).
-define(CONF_TERM_MODULE, configure_terminal_module).
-define(LOGIN_MODULE, login_module).
-define(LOGOUT_MODULE, logout_module).
-define(INTERFACE_MODULE, interface_module).
-define(IFRANGE_MODULE, interface_range_module).
-define(VLAN_MODULE, vlan_module).
-define(NOVLAN_MODULE, novlan_module).
-define(SWACCESS_VLAN_MODULE, swaccess_vlan_module).
-define(NOSWACCESS_VLAN_MODULE, noswaccess_vlan_module).
-define(NAME_MODULE, name_module).
-define(NONAME_MODULE, noname_module).
-define(END_MODULE, end_module).
-define(EXIT_MODULE, exit_module).
-define(SHOW_VLAN_MODULE, show_vlan_module).

-define(ALL_MODULES, [?PING_MODULE,
                      ?CONF_TERM_MODULE,
                      ?LOGIN_MODULE,
                      ?LOGOUT_MODULE,
                      ?INTERFACE_MODULE,
                      ?IFRANGE_MODULE,
                      ?VLAN_MODULE,
                      ?NOVLAN_MODULE,
                      ?SWACCESS_VLAN_MODULE,
                      ?NOSWACCESS_VLAN_MODULE,
                      ?NAME_MODULE,
                      ?NONAME_MODULE,
                      ?END_MODULE,
                      ?EXIT_MODULE,
                      ?SHOW_VLAN_MODULE]).