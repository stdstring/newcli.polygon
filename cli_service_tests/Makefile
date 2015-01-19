EBIN=ebin

COMMON_EBIN = ../cli_common/ebin
SRC_EBIN = ../cli_service/ebin

SOURCE=src/code_generator/cli_fsm_mock.erl \
       src/code_generator/client_handler_mock.erl \
       src/code_generator/command_execution_checker_mock.erl \
       src/code_generator/command_mock.erl \
       src/code_generator/io_buffer_mock.erl \
       src/test_commands/config_terminal_test_command.erl \
       src/test_commands/end_test_command.erl \
       src/test_commands/exit_test_command.erl \
       src/test_commands/interface_test_command.erl \
       src/test_commands/interface_range_test_command.erl \
       src/test_commands/login_test_command.erl \
       src/test_commands/logout_test_command.erl \
       src/test_commands/name_test_command.erl \
       src/test_commands/noname_test_command.erl \
       src/test_commands/noswitchport_vlan_test_command.erl \
       src/test_commands/novlan_test_command.erl \
       src/test_commands/ping_test_command.erl \
       src/test_commands/show_vlan_test_command.erl \
       src/test_commands/switchport_vlan_test_command.erl \
       src/test_commands/vlan_test_command.erl \
       src/mock_server.erl

TEST_SOURCE=src/code_generator/code_generator_tests.erl \
            src/code_generator/dynamic_code_generation_tests.erl \
            src/command_config/name_search_config_tests.erl \
            src/utils/crypto_utils_tests.erl \
            src/utils/erlang_term_utils_tests.erl \
            src/utils/list_utils_tests.erl \
            src/utils/string_utils_tests.erl

INCLUDE_FLAGS=-I include -I ../cli_service/include -I ../cli_command_parser_include

TARGETS = $(SOURCE:%.erl=$(EBIN)/%.beam)

TEST_TARGETS = $(TEST_SOURCE:%.erl=$(EBIN)/%.beam)

TESTS_LIST = $(basename $(notdir $(TEST_SOURCE)))

$(EBIN)/%.beam: %.erl
	erlc -pa $(EBIN) -pa $(COMMON_EBIN) -Werror -b beam -o $(EBIN) $(INCLUDE_FLAGS) $<

all: pre_build $(TARGETS) $(TEST_TARGETS)

test: all
	for TEST_NAME in $(TESTS_LIST); do erl -noshell -pa $(EBIN) $(SRC_EBIN) -eval "eunit:test([$$TEST_NAME], [verbose])" -s init stop; done

pre_build: clean
	$(shell if [ ! -d "./$(EBIN)/" ]; then mkdir $(EBIN); fi)

clean:
	$(shell rm -rf $(EBIN)/*)