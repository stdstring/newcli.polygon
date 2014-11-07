%% common definitions

-record(token, {type = undefined, value = undefined}).
-record(terminal, {type = value, value = undefined}).
-record(nonterminal, {name = ""}).