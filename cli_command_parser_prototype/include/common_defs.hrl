%% common definitions

-record(global_state, {syntax_table = [] :: [term()], name_table = [] :: [term()]}).

-record(frame_item, {type = undefined, value= undefined}).
-record(command_frame, {items = [] :: [#frame_item{}]}).
-record(process_state, {current_frame = undefined :: undefined | #command_frame{},
                        binary_code = undefined :: undefined | binary()}).

-record(token, {type = undefined :: 'undefined' | atom(), value = undefined :: 'undefined' | term()}).
-record(terminal, {type = undefined :: 'undefined' | atom(), value = undefined :: 'undefined' | term()}).
-record(nonterminal, {name = undefined :: 'undefined' | atom()}).

-record(token_parser_state, {current_state = undefined :: 'undefined' | atom(),
                             recognized_buffer = [] :: [byte()]}).
-record(transition, {from_state = undefined :: 'undefined' | atom(),
                     chars = [] :: [byte()],
                     to_state = undefined :: 'undefined' | atom(),
                     char_appender = undefined :: 'undefined' | fun((byte(), [byte()]) -> [byte()])}).
-record(token_parser_config, {init_state = undefined :: 'undefined' | atom(),
                              transitions = [] :: [#transition{}],
                              final_states = [] :: [atom()],
                              token_factory = undefined :: 'undefined' | fun((#token_parser_state{}) -> #token{})}).
-record(token_parser_result, {token = undefined :: 'undefined' | #token{},
                              state = undefined :: 'undefined' | #token_parser_state{}}).