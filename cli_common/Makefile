EBIN=ebin

SOURCE=src/commandline_parser.erl \
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
	if [ ! -d "./$(EBIN)/" ]; then mkdir $(EBIN); fi

test: all
	

clean:
	rm -rf $(EBIN)/*

