%% command's modules definitions

-define(PING_MODULE, ping_impl).
-define(CONF_TERM_MODULE, configure_terminal_impl).
-define(LOGIN_MODULE, login_impl).
-define(LOGOUT_MODULE, logout_impl).
-define(INTERFACE_MODULE, interface_impl).
-define(IFRANGE_MODULE, interface_range_impl).
-define(VLAN_MODULE, vlan_impl).
-define(NOVLAN_MODULE, novlan_impl).
-define(SWACCESS_VLAN_MODULE, swaccess_vlan_impl).
-define(NOSWACCESS_VLAN_MODULE, noswaccess_vlan_impl).
-define(NAME_MODULE, name_impl).
-define(NONAME_MODULE, noname_impl).
-define(END_MODULE, end_impl).
-define(EXIT_MODULE, exit_impl).
-define(SHOW_VLAN_MODULE, show_vlan_impl).

-define(FUNCTION, execute).