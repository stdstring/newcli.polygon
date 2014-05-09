SUBDIRS = cli_backend cli_frontend

all:
	for directory in $(SUBDIRS); do $(MAKE) -C $$directory; done

test:
	for directory in $(SUBDIRS); do $(MAKE) -C $$directory test; done

clean:
	for directory in $(SUBDIRS); do $(MAKE) -C $$directory clean; done
