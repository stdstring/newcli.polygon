%% io buffer definitions

-record(output, {message = "" :: string()}).
-record(error, {message = "" :: string()}).
-record(get_data, {data_type = both :: 'output' | 'error' | 'both'}).
-record(reset, {}).