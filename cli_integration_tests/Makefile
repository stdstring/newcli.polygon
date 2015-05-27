EBIN=ebin
SERVICE_EBIN=service_ebin
TERMINAL_BIN=cli_terminal_bin
COMMON_EBIN=../cli_common/ebin
COMMAND_PARSER_EBIN=../cli_command_parser/ebin

COMMANDS_SOURCE=src/test_commands/config_terminal_command.erl \
                src/test_commands/end_command.erl \
                src/test_commands/exit_command.erl \
                src/test_commands/interface_command.erl \
                src/test_commands/interface_range_command.erl \
                src/test_commands/name_command.erl \
                src/test_commands/noname_command.erl \
                src/test_commands/noswitchport_vlan_command.erl \
                src/test_commands/novlan_command.erl \
                src/test_commands/ping_command.erl \
                src/test_commands/show_vlan_command.erl \
                src/test_commands/switchport_vlan_command.erl \
                src/test_commands/vlan_command.erl
SOURCE=src/utils/cli_service_interaction_helper.erl \
       src/integration_tests_common.erl \
       src/integration_tests_manager.erl
TEST_SOURCE=src/cli_service_integration_tests.erl \
            src/cli_service_protocol_tests.erl \
            src/command_integration_tests.erl \
            src/help_integration_tests.erl \
            src/lifecycle_integration_tests.erl \
            src/login_integration_tests.erl \
            src/transition_integration_tests.erl \
            src/hosting_demo_tests.erl

INCLUDE_FLAGS=-I include -I ../cli_service/include

COMMANDS_TARGETS=$(COMMANDS_SOURCE:%.erl=$(EBIN)/%.beam)
TARGETS=$(SOURCE:%.erl=$(EBIN)/%.beam)
TEST_TARGETS=$(TEST_SOURCE:%.erl=$(EBIN)/%.beam)

TESTS_LIST = $(basename $(notdir $(TEST_SOURCE)))

$(EBIN)/%.beam: %.erl
	erlc -pa $(EBIN) -pa $(COMMON_EBIN) -Werror -b beam -o $(EBIN) $(INCLUDE_FLAGS) $<

all: pre_build $(TARGETS) $(TEST_TARGETS) $(COMMANDS_TARGETS)

test: all
	# prerequisites
	$(shell cp -f -t $(SERVICE_EBIN) service_prerequisites/*)
	# tests
	for TEST_NAME in $(TESTS_LIST); do erl -noshell -sname integration_tests_node -pa $(EBIN) $(COMMON_EBIN) $(COMMAND_PARSER_EBIN) $(SERVICE_EBIN) -eval "eunit:test([$$TEST_NAME], [verbose])" -s init stop; done

pre_build:
	$(shell if [ ! -d "./$(EBIN)/" ]; then mkdir $(EBIN); fi)

clean:
	$(shell rm -rf $(EBIN)/*)
	$(shell rm -rf $(SERVICE_EBIN)/*)
	$(shell rm -rf $(TERMINAL_BIN)/*)