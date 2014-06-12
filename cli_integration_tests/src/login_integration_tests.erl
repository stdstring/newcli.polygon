-module(login_integration_tests).

-include_lib("eunit/include/eunit.hrl").

-include("integration_tests_defs.hrl").

-define(LOGIN, "@CliDemo>login:password:").

integration_test_() ->
    integration_tests_common:create_tests_entry([
    	{["login", "guest", "idclip", "logout"], [?LOGIN, "some greeting message", "guest@CliDemo>You are logged out."], "login: user login and logout"},
    	{["login", "root", "iddqd", "logout"], [?LOGIN, "some greeting message", "root@CliDemo#You are logged out."], "login: admin login and logout"},
    	{["login", "guest", "666"], [?LOGIN, "Login's attempt is failed due to the following: {authentication_fail,bad_password}", "Command execution failed. Return code is 255"], "login: bad password"},
    	{["login", "other_user", "666"], [?LOGIN, "Login's attempt is failed due to the following: {authentication_fail,unknown_username}", "Command execution failed. Return code is 255"], "login: unknown login"}]).