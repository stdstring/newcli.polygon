CC=g++
CFLAGS=-g -std=gnu++11 -Wall
LFLAGS=

BIN=bin

HEADER=include/client_state.h\
       include/cterm_ptr.h\
       include/eterm_ptr.h\
       include/message.h\
       include/message_serialization.h\
       include/server_interaction.h\
       include/signal_mask_holder.h\
       include/signal_safe_executer.h

SOURCE=src/cli_terminal.cpp\
       src/message_serialization.cpp\
       src/server_interaction.cpp

EXTERNAL_INCLUDE=-I/usr/include/readline/\
                 -Iinclude/

EXTERNAL_LIB=-L/usr/lib/x86_64-linux-gnu -lreadline -lhistory\
             -L/usr/lib/erlang/lib/erl_interface-3.7.13/lib -lerl_interface -lei\
             -lpthread

all: pre_build
	$(CC) $(CFLAGS) $(EXTERNAL_INCLUDE) -o $(BIN)/cli_terminal $(HEADER) $(SOURCE) $(EXTERNAL_LIB)

pre_build: clean
	$(shell if [ ! -d "./$(BIN)/" ]; then mkdir $(BIN); fi)

clean:
	$(shell rm -rf $(BIN)/*)
