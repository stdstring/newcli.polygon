#! /bin/bash
erl -noshell -pa /usr/local/lib/cli_service -eval "application:start(cli_service_application)"