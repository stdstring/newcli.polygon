%% token definitions

-define(WORD_TOKEN(Value), #token{type = word, value = Value}).
%%-define(DATA_TOKEN(Value), #token{type = data, value = Value}).
-define(STRING_TOKEN(Value), #token{type = string, value = Value}).