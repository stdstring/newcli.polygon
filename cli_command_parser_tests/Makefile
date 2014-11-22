EBIN=ebin

SRC_EBIN = ../cli_command_parser/ebin

SOURCE=src/lex_analyzer_config.erl \
       src/name_search_config.erl

TEST_SOURCE=src/char_category_tests.erl \
            src/lex_analyzer_tests.erl \
            src/name_search_tests.erl

INCLUDE_FLAGS=-I include -I ../cli_command_parser_include

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