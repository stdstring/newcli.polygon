%% mock definitions

-record(expectation, {source = undefined :: 'undefined' | term(),
                      func = undefined :: 'undefined' | atom(),
                      args = [] :: ['any' | term()],
                      result = undefined :: 'undefined' | term()}).