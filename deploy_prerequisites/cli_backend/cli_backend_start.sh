#! /bin/bash
erl -noshell -sname backend_node -pa /usr/local/lib/backend -eval "application:start(cli_backend_application)"