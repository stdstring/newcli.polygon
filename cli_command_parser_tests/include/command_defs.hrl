%% module definitions

-define(PING_COMMAND, ping).
-define(CONF_TERM_COMMAND, configure_terminal).
-define(LOGIN_COMMAND, login).
-define(LOGOUT_COMMAND, logout).
-define(INTERFACE_COMMAND, interface).
-define(IFRANGE_COMMAND, interface_range).
-define(VLAN_COMMAND, vlan).
-define(NOVLAN_COMMAND, novlan).
-define(SWACCESS_VLAN_COMMAND, swaccess_vlan).
-define(NOSWACCESS_VLAN_COMMAND, noswaccess_vlan).
-define(NAME_COMMAND, name).
-define(NONAME_COMMAND, noname).
-define(END_COMMAND, 'end').
-define(EXIT_COMMAND, exit).
-define(SHOW_VLAN_COMMAND, show_vlan).

-define(ALL_COMMANDS, [?PING_COMMAND,
                       ?CONF_TERM_COMMAND,
                       ?LOGIN_COMMAND,
                       ?LOGOUT_COMMAND,
                       ?INTERFACE_COMMAND,
                       ?IFRANGE_COMMAND,
                       ?VLAN_COMMAND,
                       ?NOVLAN_COMMAND,
                       ?SWACCESS_VLAN_COMMAND,
                       ?NOSWACCESS_VLAN_COMMAND,
                       ?NAME_COMMAND,
                       ?NONAME_COMMAND,
                       ?END_COMMAND,
                       ?EXIT_COMMAND,
                       ?SHOW_VLAN_COMMAND]).