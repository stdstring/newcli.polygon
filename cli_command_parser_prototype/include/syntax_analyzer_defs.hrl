%% syntax analyzer definitions

-define(COMMAND, #nonterminal{name = command}).
-define(ARGS, #nonterminal{name = args}).

-define(WORD_TERM, #terminal{type = word, value = undefined}).
-define(STRING_TERM, #terminal{type = string, value = undefined}).
-define(END_TERM, #terminal{type = 'end', value = ""}).

-define(EXEC_CONTEXT_MODULE, command_exec_context).
-define(EXEC_CONTEXT_FUNCTION, execute).