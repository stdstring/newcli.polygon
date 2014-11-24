%% frame definitions

-record(frame_item, {type = undefined :: atom(), value= undefined :: term()}).
-record(command_frame, {items = [] :: [#frame_item{}]}).