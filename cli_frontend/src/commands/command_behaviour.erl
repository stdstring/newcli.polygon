%% @author stdstring

-module(command_behaviour).

-include("common_defs.hrl").

%% get command name
-callback get_name() -> Name :: atom().

%% get command body
-callback get_command_body() -> [string()].

%% create command's instance
-callback create(CommandLineRest :: string()) -> {'ok', Command :: pid()} | {'error', Reason :: term()}.

%% synchronous executing
-callback execute(Command :: pid(), ExecutionState :: #execution_state{}) -> {ReturnCode :: integer(), ExecutionState :: #execution_state{}}.