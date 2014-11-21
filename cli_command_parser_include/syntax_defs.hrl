%% syntax definitions

-record(terminal, {type = undefined :: 'undefined' | atom(), value = undefined :: 'undefined' | term()}).
-record(nonterminal, {name = undefined :: 'undefined' | atom()}).