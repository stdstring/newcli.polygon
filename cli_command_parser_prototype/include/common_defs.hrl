%% common definitions

-record(frame_item, {type = undefined, value= undefined}).
-record(command_frame, {items = [] :: [#frame_item{}]}).
-record(process_state, {current_frame = undefined}).

-record(token, {type = undefined :: 'undefined' | atom(), value = undefined :: 'undefined' | term()}).
-record(terminal, {type = undefined :: 'undefined' | atom(), value = undefined :: 'undefined' | term()}).
-record(nonterminal, {name = undefined :: 'undefined' | atom()}).

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