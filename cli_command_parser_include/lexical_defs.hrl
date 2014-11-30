%% lexical definitions

-record(token, {type = undefined :: 'undefined' | atom(), value = undefined :: 'undefined' | term()}).

-define(TOKEN(Type, Value), #token{type = Type, value = Value}).
-define(WHITESPACE_TOKEN, #token{type = whitespace, value = ""}).
-define(END_TOKEN, #token{type = 'end', value = ""}).

-define(EOF_CHAR, 0).

-record(transition, {from_state = undefined :: 'undefined' | atom(),
                     %%char_predicate = undefined :: 'undefined' | fun((char()) -> boolean()),
                     char_predicate = undefined,
                     to_state = undefined :: 'undefined' | atom(),
                     %%char_appender = undefined :: 'undefined' | fun((char(), string()) -> string())}).
                     char_appender = undefined}).

-record(token_parser_config, {init_state = undefined :: 'undefined' | atom(),
                              transitions = [] :: [#transition{}],
                              final_states = [] :: [atom()],
                              %%token_factory = undefined :: 'undefined' | fun((#token_parser_state{}) -> #token{})}).
                              token_factory = undefined}).

-record(lex_analyzer_config, {token_parsers_config = [] :: [#token_parser_config{}],
                              skip_whitespaces = false :: boolean()}).

-record(token_parser_state, {current_state = undefined :: 'undefined' | atom(), recognized_buffer = [] :: string()}).