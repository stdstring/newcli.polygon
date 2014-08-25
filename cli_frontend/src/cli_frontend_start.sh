#! /bin/bash
erl -noshell -sname frontend_node -eval "application:start(cli_frontend_application)"