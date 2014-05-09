EBIN=ebin

SOURCE=src/commands/command_behaviour.erl \
       src/commands/config_terminal_command.erl \
       src/commands/end_command.erl \
       src/commands/exit_command.erl \
       src/commands/interface_command.erl \
       src/commands/interface_range_command.erl \
       src/commands/login_command.erl \
       src/commands/logout_command.erl \
       src/commands/name_vlan_command.erl \
       src/commands/no_name_vlan_command.erl \
       src/commands/no_switchport_vlan_command.erl \
       src/commands/no_vlan_command.erl \
       src/commands/ping_command.erl \
       src/commands/show_vlan_command.erl \
       src/commands/switchport_vlan_command.erl \
       src/commands/vlan_command.erl \
       src/authentication_service_behaviour.erl \
       src/authentication_service.erl \
       src/authorization_service_behaviour.erl \
       src/authorization_service.erl \
       src/cli_backend_application.erl \
       src/cli_backend_supervisor.erl \
       src/cli_fsm_behaviour.erl \
       src/cli_fsm.erl \
       src/client_input_endpoint.erl \
       src/client_input_supervisor.erl \
       src/command_execution_checker.erl \
       src/command_execution_context.erl \
       src/command_parser.erl \
       src/command_parser_fsm.erl \
       src/commandline_parser.erl \
       src/config_reader.erl \
       src/config_utils.erl \
       src/erlang_term_utils.erl \
       src/global_input_endpoint.erl \
       src/logic_utils.erl \
       src/output_endpoint.erl

COPY=src/cli_backend_application.app \
     src/cli_backend_start.sh

INCLUDE_FLAGS=-I include

TARGETS = $(SOURCE:%.erl=$(EBIN)/%.beam)

$(EBIN)/%.beam: %.erl
	erlc -pa $(EBIN) -Werror -b beam -o $(EBIN) $(INCLUDE_FLAGS) $<

all: $(TARGETS) post_build
	

post_build:
	cp -f -t $(EBIN) $(COPY)

test:


clean:
	rm -f $(EBIN)/*
