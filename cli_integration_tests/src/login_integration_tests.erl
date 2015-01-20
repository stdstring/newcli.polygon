%% @author std-string

-module(login_integration_tests).

-include_lib("eunit/include/eunit.hrl").

-include("integration_tests_defs.hrl").

-define(BAD_PASSWORD, "Login's attempt is failed due to the following: bad password").
-define(UNKNOWN_USER, "Login's attempt is failed due to the following: unknown user").

integration_test_() ->
%%    integration_tests_common:create_tests_entry([
%%        {"login: user login and logout",
%%         ["login", "guest", "idclip", "logout"],
%%         ?GUEST_LOGIN ++ [?GREETING] ++ ?GUEST_LOGOUT},
%%        {"login: admin login and logout",
%%         ["login", "root", "iddqd", "logout"],
%%         ?ADMIN_LOGIN ++ [?GREETING] ++ ?ADMIN_LOGOUT},
%%        {"login: bad password",
%%         ["login", "guest", "666"],
%%         ?GUEST_LOGIN ++ [?BAD_PASSWORD, ?COMMAND_FAIL, "@CliDemo>"]},
%%        {"login: unknown login",
%%         ["login", "other_user", "666"],
%%         ["@CliDemo>login", "login:other_user", "password:", ?UNKNOWN_USER, ?COMMAND_FAIL, "@CliDemo>"]}]).
    integration_tests_common:create_tests_entry([
        {"login: user login and logout",
         ["login", "guest", "idclip", "logout"],
         ?LOGIN("guest") ++ [?GREETING] ++ ?GUEST_LOGOUT},
        {"login: admin login and logout",
         ["login", "root", "iddqd", "logout"],
         ?LOGIN("root") ++ [?GREETING] ++ ?ADMIN_LOGOUT},
        {"login: bad password",
         ["login", "guest", "666"],
         ?LOGIN("guest") ++ [?BAD_PASSWORD, ?COMMAND_FAIL, "@CliDemo>"]},
        {"login: unknown login",
         ["login", "other_user", "666"],
         ?LOGIN("other_user") ++ [?UNKNOWN_USER, ?COMMAND_FAIL, "@CliDemo>"]}]).