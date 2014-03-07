%% @author stdstring

-module(command_behaviour).

%% get command help
-callback get_help() -> Help :: string().

%% create command's instance
-callback create(CommandLineRest :: string()) -> CommandPid :: pid().

%% synchronous executing
-callback execute(Command :: pid(), Stdout :: pid(), Stderr :: pid()) -> ReturnCode :: integer().