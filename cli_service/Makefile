EBIN=ebin

COMMON_EBIN = ../cli_common/ebin

SOURCE=src/client_handler/client_downtime_timer.erl \
       src/client_handler/client_handler.erl \
       src/client_handler/client_handler_helper.erl \
       src/client_handler/command_executor.erl \
       src/command/code_generator.erl \
       src/command/code_generator_helper.erl \
       src/command/command_execution_checker.erl \
       src/command/command_factory.erl \
       src/command/command_helper.erl \
       src/command/command_search.erl \
       src/command/io_buffer.erl \
       src/command_common/login_command.erl \
       src/command_common/logout_command.erl \
       src/command/module_name_generator.erl \
       src/command_config/lex_analyzer_config.erl \
       src/command_config/name_search_config.erl \
       src/command_config/name_search_factory.erl \
       src/command_config/syntax_analyzer_config.erl \
       src/fsm/cli_fsm_behaviour.erl \
       src/fsm/cli_fsm.erl \
       src/services/authentication_service_behaviour.erl \
       src/services/authentication_service.erl \
       src/services/authorization_service_behaviour.erl \
       src/services/authorization_service.erl \
       src/utils/commandline_parser.erl \
       src/utils/crypto_utils.erl \
       src/utils/erlang_term_utils.erl \
       src/utils/list_utils.erl \
       src/utils/name_search_helper.erl \
       src/utils/string_utils.erl \
       src/cli_service_application.erl \
       src/cli_service_supervisor.erl \
       src/cli_terminal_endpoint.erl \
       src/cli_terminal_listen_endpoint.erl \
       src/cli_terminal_listen_supervisor.erl \
       src/cli_terminal_supervisor.erl \
       src/config_reader.erl \
       src/prompt_factory.erl

COPY=src/cli_service_application.app

INCLUDE_FLAGS=-I include -I ../cli_command_parser_include

TARGETS = $(SOURCE:%.erl=$(EBIN)/%.beam)

$(EBIN)/%.beam: %.erl
	erlc -pa $(EBIN) -pa $(COMMON_EBIN) -Werror -b beam -o $(EBIN) $(INCLUDE_FLAGS) $<

all: pre_build $(TARGETS) post_build

pre_build: clean
	$(shell if [ ! -d "./$(EBIN)/" ]; then mkdir $(EBIN); fi)

post_build:
	$(shell cp -f -t $(EBIN) $(COPY))

clean:
	$(shell rm -rf $(EBIN)/*)