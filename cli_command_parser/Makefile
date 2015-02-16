EBIN=ebin

SOURCE=src/char_category.erl \
       src/command_parser.erl \
       src/frame_item_search.erl \
       src/lex_analyzer.erl \
       src/name_search.erl \
       src/name_search_helper.erl \
       src/syntax_analyzer.erl \
       src/token_parser.erl

INCLUDE_FLAGS=-I include -I ../cli_command_parser_include

TARGETS = $(SOURCE:%.erl=$(EBIN)/%.beam)

$(EBIN)/%.beam: %.erl
	erlc -pa $(EBIN) -Werror -b beam -o $(EBIN) $(INCLUDE_FLAGS) $<

all: pre_build $(TARGETS)

pre_build: clean
	$(shell if [ ! -d "./$(EBIN)/" ]; then mkdir $(EBIN); fi)

clean:
	$(shell rm -rf $(EBIN)/*)
