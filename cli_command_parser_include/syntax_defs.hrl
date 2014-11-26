%% syntax definitions

-record(terminal, {type = undefined :: 'undefined' | atom(), value = undefined :: 'undefined' | term()}).
-record(nonterminal, {name = undefined :: 'undefined' | atom()}).

-type syntax_process_stack() :: [#nonterminal{} | # terminal{}].
-type syntax_table_key() :: {Source :: #nonterminal{}, Token :: #token{}}.
-type syntax_production() :: [#nonterminal{} | #terminal{}].
%%-type syntax_production_action() :: fun(([], #syntax_process_state{}, #token{}) -> ({'true', #syntax_process_state{}} | {'false', Reason :: term()})).
-type syntax_production_action() :: term().
-type syntax_table_value() :: {Production :: syntax_production(), Action :: syntax_production_action()}.
%%-type syntax_table() :: dict(syntax_table_key(), syntax_table_value()).
-type syntax_table() :: [{syntax_table_key(), syntax_table_value()}].

-record(syntax_analyzer_config, {syntax_table = [] :: syntax_table(),
                                 start_symbol = #nonterminal{} :: #nonterminal{},
                                 name_table = [] :: name_search_table()}).

-record(syntax_process_state, {current_frame = undefined :: 'undefined' | #command_frame{},
                               result =  undefined :: 'undefined' | term()}).

-define(NONTERMINAL(Name), #nonterminal{name = Name}).
-define(TERMINAL(Type, Value), #terminal{type = Type, value = Value}).

