%% @author std-string

-module(login_integration_tests).

-include_lib("eunit/include/eunit.hrl").

-include("integration_tests_defs.hrl").

-define(BAD_PASSWORD, "Login's attempt is failed due to the following: bad password").
-define(UNKNOWN_USER, "Login's attempt is failed due to the following: unknown user").

%% ====================================================================
%% Test functions
%% ====================================================================

integration_test_() ->
    integration_tests_common:create_tests_entry([
        {"login: user login and logout",
         ["login", "guest", "idclip", "logout", "bye"],
         ?GUEST_LOGIN ++ [?GREETING] ++ ?GUEST_LOGOUT},
        {"login: admin login and logout",
         ["login", "root", "iddqd", "logout", "bye"],
         ?ADMIN_LOGIN ++ [?GREETING] ++ ?ADMIN_LOGOUT},
        {"login: bad password",
         ["login", "guest", "666", "bye"],
         ?GUEST_LOGIN ++ [?BAD_PASSWORD, ?COMMAND_FAIL] ++ ?BYE_OUTPUT},
        {"login: unknown login",
         ["login", "other_user", "666", "bye"],
         ?LOGIN("other_user") ++ [?UNKNOWN_USER, ?COMMAND_FAIL] ++ ?BYE_OUTPUT}]).

%% ====================================================================
%% Internal functions
%% ====================================================================