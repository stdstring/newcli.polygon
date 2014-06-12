%% @author stdstring

-module(command_behaviour).

-include("common_defs.hrl").

%% get command name
-callback get_name() -> atom().

%% get command body
-callback get_command_body() -> [string()].

%% synchronous executing
-callback execute(CommandLineRest :: string(), ExecutionState :: #execution_state{}) -> {ReturnCode :: integer(), ExecutionState :: #execution_state{}}.