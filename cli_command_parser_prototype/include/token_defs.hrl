%% token definitions

-define(WORD_TOKEN(Value), #token{type = word, value = Value}).
-define(STRING_TOKEN(Value), #token{type = string, value = Value}).
-define(TOKEN(Type, Value), #token{type = Type, value = Value}).
-define(WHITESPACE_TOKEN, #token{type = whitespace, value = ""}).
-define(END_TOKEN, #token{type = 'end', value = ""}).