%% frame definitions

-record(frame_item, {type = undefined :: 'undefined' | atom(), value = undefined :: 'undefined' | term()}).
-record(command_frame, {items = [] :: [#frame_item{}]}).
-record(help_command_frame, {items = [] :: [#frame_item{}], prefix = "" :: string(),  arguments = [] :: [#frame_item{}]}).

-record(argument, {type = undefined :: 'undefined' | atom(), value = undefined :: 'undefined' | term()}).
-record(command, {module = undefined :: 'undefined' | atom(), arguments = [] :: [#argument{}]}).
-record(help_exact_command, {module = undefined :: 'undefined' | atom(), arguments = [] :: [#argument{}]}).
-record(help_suitable_command, {modules = [] :: [atom()], arguments = [] :: [#argument{}]}).
%%-record(help_command, {suitable_modules = [] :: [atom()],
%%                       exact_module = undefined :: 'undefined' | atom(),
%%                       arguments = [] :: [#argument{}]}).