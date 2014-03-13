%% @author std-string

-module(authorization_service_behaviour).

-include("common_defs.hrl").

-callback authorize(User :: #user{}, CommandName :: atom()) -> boolean().