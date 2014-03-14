%% @author stdstring

-module(cli_fsm_behaviour).

-callback start(Config :: [tuple()]) -> pid().

-callback process_command(CommandName :: atom()) -> {'ok', StateName :: atom()} | {'error', ErrorReason :: string()}.

-callback get_current_state() -> {StateName :: atom(), [CommandName :: atom()]}.