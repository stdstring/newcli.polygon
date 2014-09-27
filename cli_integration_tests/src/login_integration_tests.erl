-module(login_integration_tests).

-include_lib("eunit/include/eunit.hrl").

-include("integration_tests_defs.hrl").

integration_test_() ->
    integration_tests_common:create_tests_entry([
        {["login", "guest", "idclip", "logout"],
         %"@CliDemo>login\nlogin:guest\npassword:\nsome greeting message\nguest@CliDemo>logout\nYou are logged out\n@CliDemo>"
         [?LOGIN, ?GREETING, ?GUEST_LOGOUT],
         "login: user login and logout"},
        {["login", "root", "iddqd", "logout"],
         %"@CliDemo>login\nlogin:root\npassword:\nsome greeting message\nroot@CliDemo#logout\nYou are logged out\n@CliDemo>"
         [?LOGIN, ?GREETING, ?ADMIN_LOGOUT],
         "login: admin login and logout"},
        {["login", "guest", "666"],
         %"@CliDemo>login\nlogin:guest\npassword:\nLogin's attempt is failed due to the following: {authentication_fail,bad_password}\nCommand execution failed. Return code is 255\n@CliDemo>"
         [?LOGIN, "Login's attempt is failed due to the following: {authentication_fail,bad_password}", ?COMMAND_FAIL],
         "login: bad password"},
        {["login", "other_user", "666"],
         %"@CliDemo>login\nlogin:other_user\npassword:\nLogin's attempt is failed due to the following: {authentication_fail,unknown_username}\nCommand execution failed. Return code is 255\n@CliDemo>"
         [?LOGIN, "Login's attempt is failed due to the following: {authentication_fail,unknown_username}", ?COMMAND_FAIL],
         "login: unknown login"}]).