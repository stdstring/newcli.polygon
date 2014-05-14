EBIN=ebin

SOURCE=src/string_data_utils_tests.erl

INCLUDE_FLAGS=-I include

TARGETS = $(SOURCE:%.erl=$(EBIN)/%.beam)

$(EBIN)/%.beam: %.erl
	erlc -pa $(EBIN) -Werror -b beam -o $(EBIN) $(INCLUDE_FLAGS) $<

all: pre_build $(TARGETS)
	

pre_build: clean
	if [ ! -d "./$(EBIN)/" ]; then mkdir $(EBIN); fi

clean:
	rm -rf $(EBIN)/*