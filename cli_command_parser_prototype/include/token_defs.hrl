%% token definitions

-define(WORD(Value), #token{type = word, value = Value}).
-define(STRING(Value), #token{type = string, value = Value}).
-define(TOKEN(Type, Value), #token{type = Type, value = Value}).
-define(WHITESPACE, #token{type = whitespace, value = ""}).
-define(END_TOKEN, #token{type = 'end', value = ""}).