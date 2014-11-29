%% @author stdstring

-module(authentication_service_behaviour).

-include("authentication_defs.hrl").

-callback authenticate(Username :: string(), Password :: string()) -> {'ok', #user{}} | {'error', ErrorReason :: string()}.