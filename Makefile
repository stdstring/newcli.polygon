SOURCE_SUBDIRS = cli_common cli_command_parser cli_service cli_service_test_commands cli_terminal
TEST_SUBDIRS = cli_common_tests cli_command_parser_tests cli_service_tests

COMMON_EBIN = cli_common/ebin
COMMAND_PARSER_EBIN = cli_command_parser/ebin
SERVICE_EBIN = cli_service/ebin
SERVICE_DATA = cli_service/data
SERVICE_TEST_COMMANDS_EBIN = cli_service_test_commands/ebin
TERMINAL_BIN = cli_terminal/bin

INTEGRATION_TESTS = cli_integration_tests
INTEGRATION_TESTS_SERVICE_EBIN = $(INTEGRATION_TESTS)/service_ebin
INTEGRATION_TESTS_TERMINAL_BIN = $(INTEGRATION_TESTS)/cli_terminal_bin

DEPLOY = deploy
DEPLOY_SERVICE_EBIN = cli_service_ebin
DEPLOY_SERVICE_DATA = cli_service_data
DEPLOY_TERMINAL_BIN = cli_terminal_bin
DEPLOY_PREREQ = deploy_prerequisites
DEPLOY_SERVICE_PREREQ = $(DEPLOY_PREREQ)/cli_service
DEPLOY_DOCS = docs

# documentation for deploy
DOCS=docs/CLI_Architecture.docx\
     docs/CLI_Autocomplete.docx\
     docs/CLI_Dependencies.docx\
     docs/CLI_Future.docx\
     docs/CLI_service_command_parser.docx\
     docs/CLI_service_description.docx\
     docs/CLI_terminal_comm_protocol.docx\
     docs/CLI_terminal_description

all: build

build:
	for directory in $(SOURCE_SUBDIRS); do $(MAKE) -C $$directory; done

test: all
	for directory in $(TEST_SUBDIRS); do $(MAKE) -C $$directory test; done

integration_test: all
	$(shell rm -rf $(INTEGRATION_TESTS_SERVICE_EBIN))
	$(shell mkdir $(INTEGRATION_TESTS_SERVICE_EBIN))
	$(shell rm -rf $(INTEGRATION_TESTS_TERMINAL_BIN))
	$(shell mkdir $(INTEGRATION_TESTS_TERMINAL_BIN))
	$(shell cp -f -t $(INTEGRATION_TESTS_SERVICE_EBIN) $(SERVICE_EBIN)/*)
	$(shell cp -f -t $(INTEGRATION_TESTS_TERMINAL_BIN) $(TERMINAL_BIN)/*)
	$(MAKE) -C $(INTEGRATION_TESTS) test

clean:
	for directory in $(SOURCE_SUBDIRS); do $(MAKE) -C $$directory clean; done
	for directory in $(TEST_SUBDIRS); do $(MAKE) -C $$directory clean; done
	$(MAKE) -C cli_command_parser_prototype clean
	$(MAKE) -C cli_integration_tests clean
	$(MAKE) -C cli_terminal_prototype clean
	$(shell rm -rf $(DEPLOY))

deploy: all
	$(shell rm -rf $(DEPLOY))
	$(shell mkdir -p $(DEPLOY)/$(DEPLOY_SERVICE_EBIN))
	$(shell mkdir -p $(DEPLOY)/$(DEPLOY_SERVICE_DATA))
	$(shell mkdir -p $(DEPLOY)/$(DEPLOY_TERMINAL_BIN))
	$(shell mkdir -p $(DEPLOY)/$(DEPLOY_DOCS))
	$(shell cp -f -t $(DEPLOY)/$(DEPLOY_SERVICE_EBIN) $(COMMON_EBIN)/*)
	$(shell cp -f -t $(DEPLOY)/$(DEPLOY_SERVICE_EBIN) $(COMMAND_PARSER_EBIN)/*)
	$(shell cp -f -t $(DEPLOY)/$(DEPLOY_SERVICE_EBIN) $(SERVICE_EBIN)/*)
	$(shell cp -f -t $(DEPLOY)/$(DEPLOY_SERVICE_EBIN) $(SERVICE_TEST_COMMANDS_EBIN)/*)
	$(shell cp -f -t $(DEPLOY)/$(DEPLOY_SERVICE_EBIN) $(DEPLOY_SERVICE_PREREQ)/*)
	$(shell cp -f -t $(DEPLOY)/$(DEPLOY_SERVICE_DATA) $(SERVICE_DATA)/*)
	$(shell cp -f -t $(DEPLOY)/$(DEPLOY_TERMINAL_BIN) $(TERMINAL_BIN)/*)
	$(shell cp -f -t $(DEPLOY)/ $(DEPLOY_PREREQ)/install.sh)
	# documentation
	for document in $(DOCS); do libreoffice --headless --convert-to pdf $$document --outdir $(DEPLOY)/$(DEPLOY_DOCS); done
	$(shell cp -f -t $(DEPLOY)/$(DEPLOY_DOCS) docs/*.pdf)
	# create tar
	tar -c -f $(DEPLOY)/deploy.tar -C $(DEPLOY) $(DEPLOY_SERVICE_EBIN)/ \
                                                $(DEPLOY_SERVICE_DATA)/ \
                                                $(DEPLOY_TERMINAL_BIN)/ \
                                                $(DEPLOY_DOCS)/ \
                                                ./install.sh
