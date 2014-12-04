EBIN=ebin

SOURCE=src/command/code_generator.erl \
       src/command/command_execution_checker.erl \
       src/command/command_factory.erl \
       src/command/io_buffer.erl \
       src/command/lex_analyzer_config.erl \
       src/command/syntax_analyzer_config.erl \
       src/fsm/cli_fsm_behaviour.erl \
       src/fsm/cli_fsm.erl \
       src/services/authentication_service_behaviour.erl \
       src/services/authentication_service.erl \
       src/services/authorization_service_behaviour.erl \
       src/services/authorization_service.erl \
       src/utils/config_utils.erl \
       src/utils/crypto_utils.erl \
       src/utils/erlang_term_utils.erl \
       src/cli_service_application.erl \
       src/cli_service_supervisor.erl \
       src/cli_terminal_endpoint.erl \
       src/cli_terminal_listen_endpoint.erl \
       src/cli_terminal_listen_supervisor.erl \
       src/cli_terminal_supervisor.erl \
       src/client_handler.erl \
       src/command_execution_context.erl

INCLUDE_FLAGS=-I include -I ../cli_command_parser_include

TARGETS = $(SOURCE:%.erl=$(EBIN)/%.beam)

$(EBIN)/%.beam: %.erl
	erlc -pa $(EBIN) -Werror -b beam -o $(EBIN) $(INCLUDE_FLAGS) $<

all: pre_build $(TARGETS)
	

pre_build: clean
	$(shell if [ ! -d "./$(EBIN)/" ]; then mkdir $(EBIN); fi)

clean:
	$(shell rm -rf $(EBIN)/*)