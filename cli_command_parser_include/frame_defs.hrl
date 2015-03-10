%% frame definitions

-record(frame_item, {type = undefined :: 'undefined' | atom(), value = undefined :: 'undefined' | term()}).
-record(command_frame, {items = [] :: [#frame_item{}]}).

-record(argument, {type = undefined :: 'undefined' | atom(), value = undefined :: 'undefined' | term()}).
%%-record(command, {module = undefined :: 'undefined' | atom(), arguments = [] :: [#argument{}]}).
%% TODO (std_string) : think about this solution
-record(command, {name = undefined :: 'undefined' | atom(), module = undefined :: 'undefined' | atom(), arguments = [] :: [#argument{}]}).