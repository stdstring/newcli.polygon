all:
	$(MAKE) -C cli_backend/ all
	$(MAKE) -C cli_frontend/ all

test:
	$(MAKE) -C cli_backend/ test
	$(MAKE) -C cli_frontend/ test

clean:
	$(MAKE) -C cli_backend/ clean
	$(MAKE) -C cli_frontend/ clean
