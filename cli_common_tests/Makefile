EBIN=ebin

SRC_EBIN = ../cli_common/ebin

SOURCE=src/message_reader_tests.erl

INCLUDE_FLAGS=-I include

TARGETS = $(SOURCE:%.erl=$(EBIN)/%.beam)

TESTS_LIST = $(basename $(notdir $(SOURCE)))

$(EBIN)/%.beam: %.erl
	erlc -pa $(EBIN) -Werror -b beam -o $(EBIN) $(INCLUDE_FLAGS) $<

all: pre_build $(TARGETS)

test: all
	for TEST_NAME in $(TESTS_LIST); do erl -noshell -pa $(EBIN) $(SRC_EBIN) -eval "eunit:test([$$TEST_NAME], [verbose])" -s init stop; done

pre_build: clean
	$(shell if [ ! -d "./$(EBIN)/" ]; then mkdir $(EBIN); fi)

clean:
	$(shell rm -rf $(EBIN)/*)