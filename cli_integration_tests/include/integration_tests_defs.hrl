%% definitions for integration tests

-define(INPUT_DATA, "/tmp/input").

-record(integration_test_state, {backend = undefined :: 'undefined' | port(), frontend_cmd = "" :: string()}).