EBIN=ebin

SOURCE=src/cli_service_application.erl \
       src/cli_service_supervisor.erl \
       src/cli_terminal_endpoint.erl \
       src/cli_terminal_listen_endpoint.erl \
       src/cli_terminal_listen_supervisor.erl \
       src/cli_terminal_supervisor.erl \
       src/client_handler.erl \
       src/command_execution_context.erl

INCLUDE_FLAGS=-I include

TARGETS = $(SOURCE:%.erl=$(EBIN)/%.beam)

$(EBIN)/%.beam: %.erl
	erlc -pa $(EBIN) -Werror -b beam -o $(EBIN) $(INCLUDE_FLAGS) $<

all: pre_build $(TARGETS)
	

pre_build: clean
	$(shell if [ ! -d "./$(EBIN)/" ]; then mkdir $(EBIN); fi)

clean:
	$(shell rm -rf $(EBIN)/*)