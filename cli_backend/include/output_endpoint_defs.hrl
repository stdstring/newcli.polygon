%% output_endpoint definitions:

-record(output_endpoint_state, {output_pid :: 'undefined' | pid(), output_buffer = [] :: [term()]}).