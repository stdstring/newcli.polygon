EBIN=ebin

SRC_EBIN = ../cli_common/ebin

SOURCE=src/commandline_parser_tests.erl \
       src/config_utils_tests.erl \
       src/string_data_utils_tests.erl

INCLUDE_FLAGS=-I include

TARGETS = $(SOURCE:%.erl=$(EBIN)/%.beam)

TESTS_LIST = $(basename $(notdir $(SOURCE)))

$(EBIN)/%.beam: %.erl
	erlc -pa $(EBIN) -Werror -b beam -o $(EBIN) $(INCLUDE_FLAGS) $<

all: pre_build $(TARGETS)
	

test: all
	for TEST_NAME in $(TESTS_LIST); do echo $$TEST_NAME; erl -noshell -pa $(EBIN) $(SRC_EBIN) -eval "eunit:test([$$TEST_NAME])" -s init stop; done

pre_build: clean
	if [ ! -d "./$(EBIN)/" ]; then mkdir $(EBIN); fi

clean:
	rm -rf $(EBIN)/*