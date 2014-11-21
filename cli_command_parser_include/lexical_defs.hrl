%% lexical definitions

-record(token, {type = undefined :: 'undefined' | atom(), value = undefined :: 'undefined' | term()}).

-define(TOKEN(Type, Value), #token{type = Type, value = Value}).
-define(WHITESPACE_TOKEN, #token{type = whitespace, value = ""}).
-define(END_TOKEN, #token{type = 'end', value = ""}).

-record(transition, {from_state = undefined :: 'undefined' | atom(),
                     %%char_predicate = undefined :: 'undefined' | fun((byte()) -> boolean()),
                     char_predicate = undefined,
                     to_state = undefined :: 'undefined' | atom(),
                     %%char_appender = undefined :: 'undefined' | fun((byte(), [byte()]) -> [byte()])}).
                     char_appender = undefined}).

-record(token_parser_config, {init_state = undefined :: 'undefined' | atom(),
                              transitions = [] :: [#transition{}],
                              final_states = [] :: [atom()],
                              %%token_factory = undefined :: 'undefined' | fun((#token_parser_state{}) -> #token{})}).
                              token_factory = undefined}).