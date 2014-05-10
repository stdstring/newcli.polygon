SUBDIRS = cli_backend cli_frontend

all:
	for directory in $(SUBDIRS); do $(MAKE) -C $$directory; done

test:
	for directory in $(SUBDIRS); do $(MAKE) -C $$directory test; done

clean:
	for directory in $(SUBDIRS); do $(MAKE) -C $$directory clean; done

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
