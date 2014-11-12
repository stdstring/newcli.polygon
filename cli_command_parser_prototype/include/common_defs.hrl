%% common definitions

-record(global_state, {syntax_table = [] :: [term()], name_table = [] :: [term()]}).

-record(frame_item, {type = undefined, value= undefined}).
-record(command_frame, {items = [] :: [#frame_item{}]}).
-record(process_state, {current_frame = undefined}).

-record(token, {type = undefined :: 'undefined' | atom(), value = undefined :: 'undefined' | term()}).
-record(terminal, {type = undefined :: 'undefined' | atom(), value = undefined :: 'undefined' | term()}).
-record(nonterminal, {name = undefined :: 'undefined' | atom()}).