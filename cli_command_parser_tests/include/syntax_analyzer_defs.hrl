%% syntax analyzer definitions

-define(COMMAND, ?NONTERMINAL(command)).
-define(ARGS, ?NONTERMINAL(args)).

-define(WORD_TERM, ?TERMINAL(word, undefined)).
-define(STRING_TERM, ?TERMINAL(string, undefined)).
-define(END_TERM, ?TERMINAL('end', "")).

-define(EXEC_CONTEXT_MODULE, command_exec_context).
-define(EXEC_CONTEXT_FUNCTION, execute).