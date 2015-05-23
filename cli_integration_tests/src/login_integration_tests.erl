%% @author std-string

-module(login_integration_tests).

-include_lib("eunit/include/eunit.hrl").

-include("integration_tests_defs.hrl").

-define(BAD_PASSWORD, ["Login's attempt is failed due to the following: bad password", "Count of login attempts is exceeded"]).
-define(UNKNOWN_USER, ["Login's attempt is failed due to the following: unknown user", "Count of login attempts is exceeded"]).

%% ====================================================================
%% Test functions
%% ====================================================================

%% TODO (std_string) : add additional tests with different login_attempt_count value (in different config files)
integration_test_() ->
    integration_tests_common:create_tests_entry([
        {"login: user login and logout", ["guest", "idclip", "logout"], ?GUEST_LOGIN ++ [?GREETING] ++ ?GUEST_LOGOUT},
        {"login: admin login and logout", ["root", "iddqd", "logout"], ?ADMIN_LOGIN ++ [?GREETING] ++ ?ADMIN_LOGOUT},
        {"login: bad password", ["guest", "666"], ?GUEST_LOGIN ++ ?BAD_PASSWORD},
        {"login: unknown login", ["other_user", "666"], ?LOGIN("other_user") ++ ?UNKNOWN_USER}]).

%% ====================================================================
%% Internal functions
%% ====================================================================