%% @author std-string

-module(command_behaviour).

%% get command name
-callback get_name() -> atom().

%% get command body
-callback get_command_body() -> [string()].

%% get command help
-callback get_help() -> Help :: string().

%% synchronous executing
-callback execute(Args :: [term()], Stdout :: pid(), Stderr :: pid(), ExecContext :: [{Key :: atom(), Value :: term()}]) ->
    {ReturnCode :: integer(), ExecContext :: [{Key :: atom(), Value :: term()}]}.