EBIN=ebin

SOURCE=src/cli_service_application.erl

INCLUDE_FLAGS=-I include

TARGETS = $(SOURCE:%.erl=$(EBIN)/%.beam)

$(EBIN)/%.beam: %.erl
	erlc -pa $(EBIN) -Werror -b beam -o $(EBIN) $(INCLUDE_FLAGS) $<

all: pre_build $(TARGETS)
	

pre_build: clean
	$(shell if [ ! -d "./$(EBIN)/" ]; then mkdir $(EBIN); fi)

clean:
	$(shell rm -rf $(EBIN)/*)