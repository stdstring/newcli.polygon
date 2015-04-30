%% authentication definitions

-define(MIN_ACCESS_LEVEL, 0).
-define(MAX_ACCESS_LEVEL, 15).

-record(user, {uid = -1 :: integer(), username = "" :: string(), access_level = ?MIN_ACCESS_LEVEL :: integer()}).

-record(login_success, {user = undefined :: 'undefined' | #user{}, greeting = "" :: string()}).
-record(login_fail, {reason = "" :: string()}).
-record(login_error, {reason = "" :: string()}).