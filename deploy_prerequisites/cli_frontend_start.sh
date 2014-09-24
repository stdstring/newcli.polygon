#! /bin/bash
erl -noshell -sname frontend_node -pa /usr/local/lib/frontend -eval "application:start(cli_frontend_application)"