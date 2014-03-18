%% @author stdstring

-module(command_behaviour).

%% get command name
-callback get_name() -> Name :: atom().

%% get command body
-callback get_command_body() -> [string()].

%% get command help
-callback get_help() -> Help :: string().

%% create command's instance
-callback create(CommandLineParts :: [string()], Stdout :: pid(), Stderr :: pid()) -> Command :: pid().

%% synchronous executing
-callback execute(Command :: pid()) -> ReturnCode :: integer().