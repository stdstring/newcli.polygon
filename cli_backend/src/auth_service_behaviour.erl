%% @author stdstring

-module(auth_service_behaviour).

-include("common_defs.hrl").

-callback authenticate(Username :: string(), Password :: string()) -> {'ok', #user{}} | {'error', ErrorReason :: string()}.