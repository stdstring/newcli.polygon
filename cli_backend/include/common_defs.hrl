%% common definitions:

-record(user, {uid = -1 :: integer(),
               username = "" :: string(),
               access_level = 0 :: integer()}).