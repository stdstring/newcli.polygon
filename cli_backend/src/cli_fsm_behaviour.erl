%% @author stdstring

-module(cli_fsm_behaviour).

-callback start(Config :: [tuple()]) -> pid().

-callback process_command(CommandName :: atom()) -> {'ok', StateName :: atom()} | {'error', ErrorReason :: string()}.

-callback get_statename() -> StateName :: atom().

-callback get_available_commands() -> [CommandName :: atom()].