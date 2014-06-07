EBIN=ebin
COMMON_EBIN=common_ebin
BACKEND_EBIN=backend_ebin
BACKEND_COMMANDS_EBIN=backend_commands_ebin

COMMANDS_SOURCE=backend_commands/command_behaviour.erl \
                backend_commands/config_terminal_command.erl \
                backend_commands/end_command.erl \
                backend_commands/exit_command.erl \
                backend_commands/interface_command.erl \
                backend_commands/interface_range_command.erl \
                backend_commands/login_command.erl \
                backend_commands/logout_command.erl \
                backend_commands/name_vlan_command.erl \
                backend_commands/no_name_vlan_command.erl \
                backend_commands/no_switchport_vlan_command.erl \
                backend_commands/no_vlan_command.erl \
                backend_commands/ping_command.erl \
                backend_commands/show_vlan_command.erl \
                backend_commands/switchport_vlan_command.erl \
                backend_commands/vlan_command.erl
SOURCE=src/cli_backend_life_manager.erl \
       src/external_process.erl \
       src/integration_port_example.erl       
TEST_SOURCE=src/integration_tests.erl \
            src/integration_tests_example.erl

BACKEND_COMMANDS_INCLUDE_FLAGS=-I backend_commands_include
INCLUDE_FLAGS=-I include

BACKEND_COMMANDS_TARGETS=$(COMMANDS_SOURCE:%.erl=$(BACKEND_COMMANDS_EBIN)/%.beam)
TARGETS=$(SOURCE:%.erl=$(EBIN)/%.beam)
TEST_TARGETS=$(TEST_SOURCE:%.erl=$(EBIN)/%.beam)

TESTS_LIST = $(basename $(notdir $(TEST_SOURCE)))

$(EBIN)/%.beam: %.erl
	erlc -pa $(EBIN) -Werror -b beam -o $(EBIN) $(INCLUDE_FLAGS) $<

$(BACKEND_COMMANDS_EBIN)/%.beam: %.erl
	erlc -pa $(BACKEND_COMMANDS_EBIN) -Werror -b beam -o $(BACKEND_COMMANDS_EBIN) $(BACKEND_COMMANDS_INCLUDE_FLAGS) $<

all: pre_build $(TARGETS) $(TEST_TARGETS) $(BACKEND_COMMANDS_TARGETS)
	

test: all
	# prerequisites
	$(shell cp -f -t $(BACKEND_EBIN) backend_prerequisites/*)
	$(shell cp -f -t $(BACKEND_EBIN) $(BACKEND_COMMANDS_EBIN)/*)
	$(shell cp -f $(EBIN)/cli_backend_life_manager.beam $(BACKEND_EBIN))
	# tests
	# for TEST_NAME in $(TESTS_LIST); do erl -noshell -sname integration_tests_node -pa $(EBIN) $(COMMON_EBIN) -eval "eunit:test([$$TEST_NAME], [verbose])" -s init stop; done
	# erl -noshell -sname integration_tests_node -pa $(EBIN) $(COMMON_EBIN) -s integration_port_example start -s init stop
	erl -noshell -sname integration_tests_node -pa $(EBIN) $(COMMON_EBIN) -eval "eunit:test([integration_tests_example], [verbose])" -s init stop

pre_build: clean
	$(shell if [ ! -d "./$(EBIN)/" ]; then mkdir $(EBIN); fi)
	$(shell if [ ! -d "./$(BACKEND_COMMANDS_EBIN)/" ]; then mkdir $(BACKEND_COMMANDS_EBIN); fi)

clean:
	$(shell rm -rf $(EBIN)/*)
	$(shell rm -rf $(BACKEND_COMMANDS_EBIN)/*)