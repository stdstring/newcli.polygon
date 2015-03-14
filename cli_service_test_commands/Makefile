EBIN=ebin

COMMON_EBIN = ../cli_common/ebin

SOURCE=src/config_terminal_command.erl \
       src/end_command.erl \
       src/exit_command.erl \
       src/interface_command.erl \
       src/interface_range_command.erl \
       src/name_command.erl \
       src/noname_command.erl \
       src/noswitchport_vlan_command.erl \
       src/novlan_command.erl \
       src/ping_command.erl \
       src/show_vlan_command.erl \
       src/switchport_vlan_command.erl \
       src/vlan_command.erl

INCLUDE_FLAGS=-I include

TARGETS = $(SOURCE:%.erl=$(EBIN)/%.beam)

$(EBIN)/%.beam: %.erl
	erlc -pa $(EBIN) -pa $(COMMON_EBIN) -Werror -b beam -o $(EBIN) $(INCLUDE_FLAGS) $<

all: pre_build $(TARGETS)

pre_build: clean
	$(shell if [ ! -d "./$(EBIN)/" ]; then mkdir $(EBIN); fi)

clean:
	$(shell rm -rf $(EBIN)/*)

