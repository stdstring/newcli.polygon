%% name search definitions
-type name_search_item() :: {Prefix :: string(), MinLength :: pos_integer()}.
-type name_search_table() :: [{SearchItems :: [name_search_item()], Value :: term()}].