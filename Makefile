SOURCE_SUBDIRS = cli_common cli_backend cli_frontend cli_terminal cli_command_parser cli_service
TEST_SUBDIRS = cli_common_tests cli_backend_tests cli_frontend_tests cli_command_parser_tests cli_service_tests

BACKEND_EBIN = cli_backend/ebin
BACKEND_DATA = cli_backend/data
FRONTEND_EBIN = cli_frontend/ebin
FRONTEND_DATA = cli_frontend/data
COMMON_EBIN = cli_common/ebin
TERMINAL_BIN = cli_terminal/bin

INTEGRATION_TESTS_OLD = cli_integration_tests_old
INTEGRATION_TESTS_OLD_BACKEND_EBIN = $(INTEGRATION_TESTS_OLD)/backend_ebin
INTEGRATION_TESTS_OLD_COMMON_EBIN = $(INTEGRATION_TESTS_OLD)/common_ebin
INTEGRATION_TESTS_OLD_FRONTEND_EBIN = $(INTEGRATION_TESTS_OLD)/frontend_ebin
INTEGRATION_TESTS_OLD_TERMINAL_BIN = $(INTEGRATION_TESTS_OLD)/cli_terminal_bin

DEPLOY = deploy
DEPLOY_BACKEND_EBIN = cli_backend_ebin
DEPLOY_BACKEND_DATA = cli_backend_data
DEPLOY_FRONTEND_EBIN = cli_frontend_ebin
DEPLOY_FRONTEND_DATA = cli_frontend_data
DEPLOY_TERMINAL_BIN = cli_terminal_bin
DEPLOY_PREREQ = deploy_prerequisites
DEPLOY_BACKEND_PREREQ = $(DEPLOY_PREREQ)/cli_backend
DEPLOY_FRONTEND_PREREQ = $(DEPLOY_PREREQ)/cli_frontend
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

all: build post_build
	

build:
	for directory in $(SOURCE_SUBDIRS); do $(MAKE) -C $$directory; done

post_build:
	$(shell cp -f -t $(BACKEND_EBIN) $(COMMON_EBIN)/*)
	$(shell cp -f -t $(FRONTEND_EBIN) $(COMMON_EBIN)/*)

test: all
	for directory in $(TEST_SUBDIRS); do $(MAKE) -C $$directory test; done

integration_test_old: all
	$(shell rm -rf $(INTEGRATION_TESTS_OLD_BACKEND_EBIN))
	$(shell mkdir $(INTEGRATION_TESTS_OLD_BACKEND_EBIN))
	$(shell rm -rf $(INTEGRATION_TESTS_OLD_COMMON_EBIN))
	$(shell mkdir $(INTEGRATION_TESTS_OLD_COMMON_EBIN))
	$(shell rm -rf $(INTEGRATION_TESTS_OLD_FRONTEND_EBIN))
	$(shell mkdir $(INTEGRATION_TESTS_OLD_FRONTEND_EBIN))
	$(shell rm -rf $(INTEGRATION_TESTS_OLD_TERMINAL_BIN))
	$(shell mkdir $(INTEGRATION_TESTS_OLD_TERMINAL_BIN))
	$(shell cp -f -t $(INTEGRATION_TESTS_OLD_BACKEND_EBIN) $(BACKEND_EBIN)/*)
	$(shell cp -f -t $(INTEGRATION_TESTS_OLD_COMMON_EBIN) $(COMMON_EBIN)/*)
	$(shell cp -f -t $(INTEGRATION_TESTS_OLD_FRONTEND_EBIN) $(FRONTEND_EBIN)/*)
	$(shell cp -f -t $(INTEGRATION_TESTS_OLD_TERMINAL_BIN) $(TERMINAL_BIN)/*)
	$(MAKE) -C $(INTEGRATION_TESTS_OLD) test

clean:
	for directory in $(SOURCE_SUBDIRS); do $(MAKE) -C $$directory clean; done
	for directory in $(TEST_SUBDIRS); do $(MAKE) -C $$directory clean; done
	$(MAKE) -C cli_command_parser_prototype clean
	$(MAKE) -C cli_integration_tests_old clean

deploy: all
	$(shell rm -rf $(DEPLOY))
	$(shell mkdir -p $(DEPLOY)/$(DEPLOY_BACKEND_EBIN))
	$(shell mkdir -p $(DEPLOY)/$(DEPLOY_BACKEND_DATA))
	$(shell mkdir -p $(DEPLOY)/$(DEPLOY_FRONTEND_EBIN))
	$(shell mkdir -p $(DEPLOY)/$(DEPLOY_FRONTEND_DATA))
	$(shell mkdir -p $(DEPLOY)/$(DEPLOY_TERMINAL_BIN))
	$(shell mkdir -p $(DEPLOY)/$(DEPLOY_DOCS))
	$(shell cp -f -t $(DEPLOY)/$(DEPLOY_BACKEND_EBIN) $(BACKEND_EBIN)/*)
	$(shell cp -f -t $(DEPLOY)/$(DEPLOY_BACKEND_EBIN) $(DEPLOY_BACKEND_PREREQ)/*)
	$(shell cp -f -t $(DEPLOY)/$(DEPLOY_BACKEND_DATA) $(BACKEND_DATA)/*)
	$(shell cp -f -t $(DEPLOY)/$(DEPLOY_FRONTEND_EBIN) $(FRONTEND_EBIN)/*)
	$(shell cp -f -t $(DEPLOY)/$(DEPLOY_FRONTEND_EBIN) $(DEPLOY_FRONTEND_PREREQ)/*)
	$(shell cp -f -t $(DEPLOY)/$(DEPLOY_FRONTEND_DATA) $(FRONTEND_DATA)/*)
	$(shell cp -f -t $(DEPLOY)/$(DEPLOY_TERMINAL_BIN) $(TERMINAL_BIN)/*)
	$(shell cp -f -t $(DEPLOY)/ $(DEPLOY_PREREQ)/install.sh)
	# documentation
	for document in $(DOCS); do libreoffice --headless --convert-to pdf $$document --outdir $(DEPLOY)/$(DEPLOY_DOCS); done
	$(shell cp -f -t $(DEPLOY)/$(DEPLOY_DOCS) docs/*.pdf)
	# create tar
	tar -c -f $(DEPLOY)/deploy.tar -C $(DEPLOY) $(DEPLOY_BACKEND_EBIN)/ \
                                                $(DEPLOY_BACKEND_DATA)/ \
                                                $(DEPLOY_FRONTEND_EBIN)/ \
                                                $(DEPLOY_FRONTEND_DATA)/ \
                                                $(DEPLOY_TERMINAL_BIN)/ \
                                                $(DEPLOY_DOCS)/ \
                                                ./install.sh
