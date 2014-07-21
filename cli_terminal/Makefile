CC=g++
CFLAGS=-g -std=gnu++11 -Wall
LFLAGS=

BIN=bin

SOURCE=src/cli_terminal.cpp

LIBS=-L/usr/lib/x86_64-linux-gnu -lreadline -lhistory\
     -L/usr/lib/erlang/lib/erl_interface-3.7.13/lib -lerl_interface -lei\
     -lpthread

all: pre_build
	$(CC) $(CFLAGS) -o $(BIN)/cli_terminal $(SOURCE) $(LIBS)

pre_build: clean
	$(shell if [ ! -d "./$(BIN)/" ]; then mkdir $(BIN); fi)

clean:
	$(shell rm -rf $(BIN)/*)
