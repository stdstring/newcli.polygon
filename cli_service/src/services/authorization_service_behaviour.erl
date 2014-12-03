%% @author std-string

-module(authorization_service_behaviour).

-include("authentication_defs.hrl").

-callback authorize_command(User :: #user{}, CommandName :: atom()) -> {'authorization_result', 'access_allowed' | 'access_denied'} | {'authorization_fail', Reason :: atom()}.
-callback authorize_commands(User :: #user{}, CommandNames :: [atom()]) -> [atom()].