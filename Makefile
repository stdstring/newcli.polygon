SOURCE_SUBDIRS = cli_common cli_backend cli_frontend cli_terminal_experiments
TEST_SUBDIRS = cli_common_tests cli_backend_tests cli_frontend_tests

BACKEND_EBIN = cli_backend/ebin
BACKEND_DATA = cli_backend/data
FRONTEND_EBIN = cli_frontend/ebin
FRONTEND_DATA = cli_frontend/data
COMMON_EBIN = cli_common/ebin

INTEGRATION_TESTS = cli_integration_tests
INTEGRATION_TESTS_BACKEND_EBIN = $(INTEGRATION_TESTS)/backend_ebin
INTEGRATION_TESTS_COMMON_EBIN = $(INTEGRATION_TESTS)/common_ebin
INTEGRATION_TESTS_FRONTEND_EBIN = $(INTEGRATION_TESTS)/frontend_ebin

DEPLOY = deploy
DEPLOY_BACKEND_EBIN = cli_backend_ebin
DEPLOY_BACKEND_DATA = cli_backend_data
DEPLOY_FRONTEND_EBIN = cli_frontend_ebin
DEPLOY_FRONTEND_DATA = cli_frontend_data

all: build post_build
	

build:
	for directory in $(SOURCE_SUBDIRS); do $(MAKE) -C $$directory; done

post_build:
	$(shell cp -f -t $(BACKEND_EBIN) $(COMMON_EBIN)/*)
	$(shell cp -f -t $(FRONTEND_EBIN) $(COMMON_EBIN)/*)

test: all
	for directory in $(TEST_SUBDIRS); do $(MAKE) -C $$directory test; done

integration_test: all
	$(shell if [ ! -d "$(INTEGRATION_TESTS_BACKEND_EBIN)" ]; then mkdir $(INTEGRATION_TESTS_BACKEND_EBIN); fi)
	$(shell if [ ! -d "$(INTEGRATION_TESTS_COMMON_EBIN)" ]; then mkdir $(INTEGRATION_TESTS_COMMON_EBIN); fi)
	$(shell if [ ! -d "$(INTEGRATION_TESTS_FRONTEND_EBIN)" ]; then mkdir $(INTEGRATION_TESTS_FRONTEND_EBIN); fi)
	$(shell rm -rf $(INTEGRATION_TESTS_BACKEND_EBIN)/*)
	$(shell rm -rf $(INTEGRATION_TESTS_COMMON_EBIN)/*)
	$(shell rm -rf $(INTEGRATION_TESTS_FRONTEND_EBIN)/*)
	$(shell cp -f -t $(INTEGRATION_TESTS_BACKEND_EBIN) $(BACKEND_EBIN)/*)
	$(shell cp -f -t $(INTEGRATION_TESTS_COMMON_EBIN) $(COMMON_EBIN)/*)
	$(shell cp -f -t $(INTEGRATION_TESTS_FRONTEND_EBIN) $(FRONTEND_EBIN)/*)
	$(MAKE) -C $(INTEGRATION_TESTS) test

clean:
	for directory in $(SOURCE_SUBDIRS); do $(MAKE) -C $$directory clean; done

deploy: all
	$(shell rm -rf $(DEPLOY))
	$(shell mkdir -p $(DEPLOY)/$(DEPLOY_BACKEND_EBIN))
	$(shell mkdir -p $(DEPLOY)/$(DEPLOY_BACKEND_DATA))
	$(shell mkdir -p $(DEPLOY)/$(DEPLOY_FRONTEND_EBIN))
	$(shell mkdir -p $(DEPLOY)/$(DEPLOY_FRONTEND_DATA))
	$(shell cp -f -t $(DEPLOY)/$(DEPLOY_BACKEND_EBIN) $(BACKEND_EBIN)/*)
	$(shell cp -f -t $(DEPLOY)/$(DEPLOY_BACKEND_DATA) $(BACKEND_DATA)/*)
	$(shell cp -f -t $(DEPLOY)/$(DEPLOY_FRONTEND_EBIN) $(FRONTEND_EBIN)/*)
	$(shell cp -f -t $(DEPLOY)/$(DEPLOY_FRONTEND_DATA) $(FRONTEND_DATA)/*)
	$(shell tar -c -f $(DEPLOY)/deploy.tar -C $(DEPLOY) $(DEPLOY_BACKEND_EBIN)/ $(DEPLOY_BACKEND_DATA)/ $(DEPLOY_FRONTEND_EBIN)/ $(DEPLOY_FRONTEND_DATA)/)
