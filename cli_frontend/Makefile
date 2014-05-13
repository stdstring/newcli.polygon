EBIN=ebin

SOURCE=src/commands/command_behaviour.erl \
       src/commands/backend_command.erl \
       src/commands/help_command.erl \
       src/commands/login_command.erl \
       src/commands/logout_command.erl \
       src/autocomplete_factory.erl \
       src/cli_frontend_application.erl \
       src/command_execution_context.erl \
       src/command_parser.erl \
       src/command_parser_fsm.erl \
       src/commandline_parser.erl \
       src/config_reader.erl \
       src/config_utils.erl \
       src/crypto_utils.erl \
       src/erlang_term_utils.erl \
       src/logic_utils.erl \
       src/string_data_utils.erl

INCLUDE_FLAGS=-I include

TARGETS = $(SOURCE:%.erl=$(EBIN)/%.beam)

$(EBIN)/%.beam: %.erl
	erlc -pa $(EBIN) -Werror -b beam -o $(EBIN) $(INCLUDE_FLAGS) $<

all: pre_build $(TARGETS)
	

pre_build:
	if [ ! -d "./$(EBIN)/" ]; then mkdir $(EBIN); fi

test: all


clean:
	rm -f $(EBIN)/*
