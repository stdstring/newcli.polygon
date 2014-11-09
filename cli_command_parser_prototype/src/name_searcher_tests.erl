%% @author std-string

-module(name_searcher_tests).

-include_lib("eunit/include/eunit.hrl").

%%-export([conf_terminal_impl/1,
%%         end_impl/1,
%%         exit_impl/1,
%%         interface_impl/1,
%%         irange_impl/1,
%%         login_impl/1,
%%         logout_impl/1,
%%         name_impl/1,
%%         noname_impl/1,
%%         noswaccess_vlan_impl/1,
%%         novlan_impl/1,
%%         ping_impl/1,
%%         show_vlan_impl/1,
%%         swaccess_vlan_impl/1,
%%         vlan_impl/1]).

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

%% ====================================================================
%% Test functions
%% ====================================================================

search_test_() ->
    NameTable = create_name_table(),
    [success_check("search ping XXX", ?PING_MODULE, ["XXX"], ["ping", "XXX"], NameTable),
     success_check("search p XXX", ?PING_MODULE, ["XXX"], ["p", "XXX"], NameTable),
     fail_check("search png XXX", ["png", "XXX"], NameTable),
     success_check("search configure terminal", ?CONF_TERM_MODULE, [], ["configure", "terminal"], NameTable),
     success_check("search c terminal", ?CONF_TERM_MODULE, [], ["c", "terminal"], NameTable),
     success_check("search c t", ?CONF_TERM_MODULE, [], ["c", "t"], NameTable),
     success_check("search interface 666", ?INTERFACE_MODULE, ["666"], ["interface", "666"], NameTable),
     success_check("search i 666", ?INTERFACE_MODULE, ["666"], ["i", "666"], NameTable),
     success_check("search interface range 666", ?IFRANGE_MODULE, ["666"], ["interface", "range", "666"], NameTable),
     success_check("search i range 666", ?IFRANGE_MODULE, ["666"], ["i", "range", "666"], NameTable),
     success_check("search interface r 666", ?IFRANGE_MODULE, ["666"], ["interface", "r", "666"], NameTable),
     success_check("search i r 666", ?IFRANGE_MODULE, ["666"], ["i", "r", "666"], NameTable),
     success_check("search name IDDQD", ?NAME_MODULE, ["IDDQD"], ["name", "IDDQD"], NameTable),
     success_check("search na IDDQD", ?NAME_MODULE, ["IDDQD"], ["na", "IDDQD"], NameTable),
     fail_check("search n IDDQD", ["n", "IDDQD"], NameTable),
     success_check("search no name", ?NONAME_MODULE, [], ["no", "name"], NameTable),
     fail_check("search n name", ["n", "name"], NameTable),
     fail_check("search unknown_command IDDQD", ["unknown_command", "IDDQD"], NameTable)].

%% ====================================================================
%% API functions
%% ====================================================================

%%conf_terminal_impl(Args) ->
%%    io:format(user, "configure terminal command~n", []),
%%    show_args(Args).

%%end_impl(Args) ->
%%    io:format(user, "end command~n", []),
%%    show_args(Args).

%%exit_impl(Args) ->
%%    io:format(user, "exit command~n", []),
%%    show_args(Args).

%%interface_impl(Args) ->
%%    io:format(user, "interface command~n", []),
%%    show_args(Args).

%%irange_impl(Args) ->
%%    io:format(user, "interface range command~n", []),
%%    show_args(Args).

%%login_impl(Args) ->
%%    io:format(user, "login command~n", []),
%%    show_args(Args).

%%logout_impl(Args) ->
%%    io:format(user, "logout command~n", []),
%%    show_args(Args).

%%name_impl(Args) ->
%%    io:format(user, "name command~n", []),
%%    show_args(Args).

%%noname_impl(Args) ->
%%    io:format(user, "no name command~n", []),
%%    show_args(Args).

%%noswaccess_vlan_impl(Args) ->
%%    io:format(user, "no switchport access vlan command~n", []),
%%    show_args(Args).

%%novlan_impl(Args) ->
%%    io:format(user, "no vlan command~n", []),
%%    show_args(Args).

%%ping_impl(Args) ->
%%    io:format(user, "ping command~n", []),
%%    show_args(Args).

%%show_vlan_impl(Args) ->
%%    io:format(user, "show vlan command~n", []),
%%    show_args(Args).

%%swaccess_vlan_impl(Args) ->
%%    io:format(user, "switchport access vlan command~n", []),
%%    show_args(Args).

%%vlan_impl(Args) ->
%%    io:format(user, "vlan command~n", []),
%%    show_args(Args).

%% ====================================================================
%% Internal functions
%% ====================================================================

create_name_table() ->
    [{[{"ping", 1}], ?PING_MODULE, ?FUNCTION},
     {[{"configure", 1}, {"terminal", 1}], ?CONF_TERM_MODULE, ?FUNCTION},
     {[{"login", 4}], ?LOGIN_MODULE, ?FUNCTION},
     {[{"logout", 4}], ?LOGOUT_MODULE, ?FUNCTION},
     {[{"interface", 1}], ?INTERFACE_MODULE, ?FUNCTION},
     {[{"interface", 1}, {"range", 1}], ?IFRANGE_MODULE, ?FUNCTION},
     {[{"vlan", 1}], ?VLAN_MODULE, ?FUNCTION},
     {[{"no", 2}, {"vlan", 1}], ?NOVLAN_MODULE, ?FUNCTION},
     {[{"switchport", 2}, {"access", 1}, {"vlan", 1}], ?SWACCESS_VLAN_MODULE, ?FUNCTION},
     {[{"no", 2}, {"switchport", 1}, {"access", 1}, {"vlan", 1}], ?NOSWACCESS_VLAN_MODULE, ?FUNCTION},
     {[{"name", 2}], ?NAME_MODULE, ?FUNCTION},
     {[{"no", 2}, {"name", 1}], ?NONAME_MODULE, ?FUNCTION},
     {[{"end", 2}], ?END_MODULE, ?FUNCTION},
     {[{"exit", 2}], ?EXIT_MODULE, ?FUNCTION},
     {[{"show", 2}, {"vlan", 1}], ?SHOW_VLAN_MODULE, ?FUNCTION}].

success_check(Description, Module, Args, Words, NameTable) ->
    {Description, ?_assertEqual({true, Module, ?FUNCTION, Args}, name_searcher:search_best(Words, NameTable))}.

fail_check(Description, Words, NameTable) ->
    {Description, ?_assertEqual(false, name_searcher:search_best(Words, NameTable))}.

%%show_args(Args) ->
%%    io:format(user, "arguments: ~p~n", [Args]).