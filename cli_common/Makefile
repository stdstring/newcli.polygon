EBIN=ebin

SOURCE=src/command_behaviour.erl \
       src/command_utils.erl \
       src/commandline_old_parser.erl \
       src/config_utils.erl \
       src/crypto_utils.erl \
       src/erlang_term_utils.erl \
       src/logic_utils.erl \
       src/message_reader.erl \
       src/string_data_utils.erl

INCLUDE_FLAGS=-I include

TARGETS = $(SOURCE:%.erl=$(EBIN)/%.beam)

$(EBIN)/%.beam: %.erl
	erlc -pa $(EBIN) -Werror -b beam -o $(EBIN) $(INCLUDE_FLAGS) $<

all: pre_build $(TARGETS)
	

pre_build: clean
	$(shell if [ ! -d "./$(EBIN)/" ]; then mkdir $(EBIN); fi)

test: all
	

clean:
	$(shell rm -rf $(EBIN)/*)

