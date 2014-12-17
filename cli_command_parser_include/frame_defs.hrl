%% frame definitions

-record(frame_item, {type = undefined :: 'undefined' | atom(), value = undefined :: 'undefined' | term()}).
-record(command_frame, {items = [] :: [#frame_item{}]}).

-record(argument, {type = undefined :: 'undefined' | atom(), value = undefined :: 'undefined' | term()}).
-record(command, {module = undefined :: 'undefined' | atom(), arguments = [] :: [#argument{}]}).