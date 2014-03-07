%% @author stdstring

-module(command_behaviour).

-callback get_help() -> Help :: string().

%% synchronous executing
-callback execute(Stdout :: pid(), Stderr :: pid()) -> ReturnCode :: integer().