%% authentication definitions

-define(MIN_ACCESS_LEVEL, 0).
-define(MAX_ACCESS_LEVEL, 15).

-record(user, {uid = -1 :: integer(), username = "" :: string(), access_level = ?MIN_ACCESS_LEVEL :: integer()}).