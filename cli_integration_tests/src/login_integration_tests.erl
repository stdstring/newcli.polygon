-module(login_integration_tests).

-include_lib("eunit/include/eunit.hrl").

-include("integration_tests_defs.hrl").

integration_test_() ->
    integration_tests_common:create_tests_entry([
    	{["login", "guest", "idclip", "logout"],
    	 [?LOGIN, ?GREETING, ?GUEST_LOGOUT],
    	 "login: user login and logout"},
    	{["login", "root", "iddqd", "logout"],
    	 [?LOGIN, ?GREETING, ?ADMIN_LOGOUT],
    	 "login: admin login and logout"},
    	{["login", "guest", "666"],
    	 [?LOGIN, "Login's attempt is failed due to the following: {authentication_fail,bad_password}", ?COMMAND_FAIL],
    	 "login: bad password"},
    	{["login", "other_user", "666"],
    	 [?LOGIN, "Login's attempt is failed due to the following: {authentication_fail,unknown_username}", ?COMMAND_FAIL],
    	 "login: unknown login"}]).