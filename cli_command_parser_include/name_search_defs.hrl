%% name search definitions
-type name_search_item() :: {Prefix :: string(), MinLength :: pos_integer()}.
-type name_search_entry() :: {SearchItems :: [name_search_item()], Value :: term()}.
-type name_search_table() :: [name_search_entry()].