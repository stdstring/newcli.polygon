#! /bin/bash
erl -noshell -sname backend_node -eval "application:start(cli_backend_application)"