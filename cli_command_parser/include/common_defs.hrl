%% common definitions

-record(token_parser_result, {token = undefined :: 'undefined' | #token{}, state = undefined :: 'undefined' | #token_parser_state{}}).