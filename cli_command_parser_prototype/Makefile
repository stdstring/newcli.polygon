EBIN=ebin

SOURCE=src/char_category.erl \
       src/char_category_tests.erl \
       src/code_generator.erl \
       src/command_parser_tests.erl \
       src/commands_example.erl \
       src/dynamic_module_tests.erl \
       src/frame_item_search.erl \
       src/frame_item_search_tests.erl \
       src/lex_analyzer.erl \
       src/lex_analyzer_config.erl \
       src/lex_analyzer_tests.erl \
       src/name_search.erl \
       src/name_search_config.erl \
       src/name_search_tests.erl \
       src/syntax_analyzer.erl \
       src/syntax_analyzer_config.erl \
       src/syntax_analyzer_tests.erl \
       src/token_parser.erl

INCLUDE_FLAGS=-I include

TARGETS = $(SOURCE:%.erl=$(EBIN)/%.beam)

$(EBIN)/%.beam: %.erl
	erlc -pa $(EBIN) -Werror -b beam -o $(EBIN) $(INCLUDE_FLAGS) $<

all: pre_build $(TARGETS)

pre_build: clean
	$(shell if [ ! -d "./$(EBIN)/" ]; then mkdir $(EBIN); fi)

test: all
	erl -noshell -pa $(EBIN) -eval "eunit:test([char_category_tests], [verbose])" -s init stop
	erl -noshell -pa $(EBIN) -eval "eunit:test([command_parser_tests], [verbose])" -s init stop
	erl -noshell -pa $(EBIN) -eval "eunit:test([dynamic_module_tests], [verbose])" -s init stop
	erl -noshell -pa $(EBIN) -eval "eunit:test([frame_item_search_tests], [verbose])" -s init stop
	erl -noshell -pa $(EBIN) -eval "eunit:test([lex_analyzer_tests], [verbose])" -s init stop
	erl -noshell -pa $(EBIN) -eval "eunit:test([name_search_tests], [verbose])" -s init stop
	erl -noshell -pa $(EBIN) -eval "eunit:test([syntax_analyzer_tests], [verbose])" -s init stop

clean:
	$(shell rm -rf $(EBIN)/*)
