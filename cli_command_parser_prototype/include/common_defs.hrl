%% common definitions

-record(command_frame_item, {type = undefined, value= undefined}).
-record(command_frame, {items = []}).
-record(process_state, {current_frame = undefined}).

-record(token, {type = undefined :: 'undefined' | atom(), value = undefined :: 'undefined' | term()}).
-record(terminal, {type = undefined :: 'undefined' | atom(), value = undefined :: 'undefined' | term()}).
-record(nonterminal, {name = undefined :: 'undefined' | atom()}).