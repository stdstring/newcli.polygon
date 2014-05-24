SOURCE_SUBDIRS = cli_common cli_backend cli_frontend
TEST_SUBDIRS = cli_common_tests cli_backend_tests cli_frontend_tests

all: build post_build
	

build:
	for directory in $(SOURCE_SUBDIRS); do $(MAKE) -C $$directory; done

post_build:
	cp -f -t cli_backend/ebin cli_common/ebin/*
	cp -f -t cli_frontend/ebin cli_common/ebin/*

test: all
	# unit & functional tests
	for directory in $(TEST_SUBDIRS); do $(MAKE) -C $$directory test; done
	# integration tests
	if [ ! -d "cli_integration_tests/backend_ebin" ]; then mkdir cli_integration_tests/backend_ebin; fi
	if [ ! -d "cli_integration_tests/common_ebin" ]; then mkdir cli_integration_tests/common_ebin; fi
	if [ ! -d "cli_integration_tests/frontend_ebin" ]; then mkdir cli_integration_tests/frontend_ebin; fi
	cp -f -t cli_integration_tests/backend_ebin cli_backend/ebin/*
	cp -f -t cli_integration_tests/common_ebin cli_common/ebin/*
	cp -f -t cli_integration_tests/frontend_ebin cli_frontend/ebin/*
	$(MAKE) -C cli_integration_tests test

clean:
	for directory in $(SOURCE_SUBDIRS); do $(MAKE) -C $$directory clean; done

deploy: all
	rm -rf deploy
	mkdir -p deploy/cli_backend_ebin
	mkdir -p deploy/cli_backend_data
	mkdir -p deploy/cli_frontend_ebin
	mkdir -p deploy/cli_frontend_data
	cp -f -t deploy/cli_backend_ebin cli_backend/ebin/*
	cp -f -t deploy/cli_backend_data cli_backend/data/*
	cp -f -t deploy/cli_frontend_ebin cli_frontend/ebin/*
	cp -f -t deploy/cli_frontend_data cli_frontend/data/*
	tar -c -f deploy/deploy.tar -C deploy cli_backend_ebin/ cli_backend_data/ cli_frontend_ebin/ cli_frontend_data/
