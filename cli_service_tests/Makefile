EBIN=ebin

SRC_EBIN = ../cli_service/ebin

SOURCE=src/code_generator/cli_fsm_mock.erl \
       src/code_generator/client_handler_mock.erl \
       src/code_generator/command_execution_checker_mock.erl \
       src/code_generator/io_buffer_mock.erl

TEST_SOURCE=src/utils/config_utils_tests.erl \
            src/utils/crypto_utils_tests.erl \
            src/utils/erlang_term_utils_tests.erl

INCLUDE_FLAGS=-I include

TARGETS = $(SOURCE:%.erl=$(EBIN)/%.beam)

TEST_TARGETS = $(TEST_SOURCE:%.erl=$(EBIN)/%.beam)

TESTS_LIST = $(basename $(notdir $(TEST_SOURCE)))

$(EBIN)/%.beam: %.erl
	erlc -pa $(EBIN) -Werror -b beam -o $(EBIN) $(INCLUDE_FLAGS) $<

all: pre_build $(TARGETS) $(TEST_TARGETS)

test: all
	for TEST_NAME in $(TESTS_LIST); do erl -noshell -pa $(EBIN) $(SRC_EBIN) -eval "eunit:test([$$TEST_NAME], [verbose])" -s init stop; done

pre_build: clean
	$(shell if [ ! -d "./$(EBIN)/" ]; then mkdir $(EBIN); fi)

clean:
	$(shell rm -rf $(EBIN)/*)