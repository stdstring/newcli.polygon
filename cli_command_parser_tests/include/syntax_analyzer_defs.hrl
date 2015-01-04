%% syntax analyzer definitions

-define(COMMAND, ?NONTERMINAL(command)).
-define(ARGS, ?NONTERMINAL(args)).

-define(WORD_TERM, ?TERMINAL(word, undefined)).
-define(STRING_TERM, ?TERMINAL(string, undefined)).
-define(END_TERM, ?TERMINAL('end', "")).

-define(WORD_ARG(Value), #argument{type = word, value = Value}).
-define(STRING_ARG(Value), #argument{type = string, value = Value}).