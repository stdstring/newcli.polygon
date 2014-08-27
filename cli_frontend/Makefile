EBIN=ebin

SOURCE=src/commands/command_behaviour.erl \
       src/commands/backend_command.erl \
       src/commands/backend_command_inner.erl \
       src/commands/help_command.erl \
       src/commands/login_command.erl \
       src/commands/logout_command.erl \
       src/autocomplete_factory.erl \
       src/client_handler.erl \
       src/command_helper.erl \
       src/cli_frontend_application.erl \
       src/cli_frontend_supervisor.erl \
       src/cli_frontend_testing.erl \
       src/cli_terminal_endpoint.erl \
       src/cli_terminal_listen_endpoint.erl \
       src/cli_terminal_listen_supervisor.erl \
       src/cli_terminal_supervisor.erl \
       src/command_execution_context.erl \
       src/command_parser.erl \
       src/command_parser_fsm.erl \
       src/commands_info_helper.erl \
       src/config_reader.erl \
       src/message_helper.erl \
       src/prompt_factory.erl

COPY=src/cli_frontend_application.app \
     src/cli_frontend_start.sh

INCLUDE_FLAGS=-I include

TARGETS = $(SOURCE:%.erl=$(EBIN)/%.beam)

$(EBIN)/%.beam: %.erl
	erlc -pa $(EBIN) -Werror -b beam -o $(EBIN) $(INCLUDE_FLAGS) $<

all: pre_build $(TARGETS) post_build
	

pre_build: clean
	$(shell if [ ! -d "./$(EBIN)/" ]; then mkdir $(EBIN); fi)

post_build:
	$(shell cp -f -t $(EBIN) $(COPY))

test: all


clean:
	$(shell rm -rf $(EBIN)/*)
