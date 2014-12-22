 %% @author stdstring

-module(command_behaviour).

-include("common_defs.hrl").

%% get command name
-callback get_name() -> atom().

%% get command body
-callback get_command_body() -> [string()].

%% get command help
-callback get_help() -> Help :: string().

%% create command's instance
%%-callback create(CommandLineRest :: string(), Stdout :: pid(), Stderr :: pid()) -> {'ok', Command :: pid()} | {'error', Reason :: term()}.

%% synchronous executing
-callback execute(Args :: [term()], Stdout :: pid(), Stderr :: pid(), ExecContext :: dict()) ->
    {ReturnCode :: integer(), ExecContext :: dict()}.