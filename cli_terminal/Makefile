CC=g++
CFLAGS=-g -std=gnu++11 -Wall
LFLAGS=

BIN=bin

HEADER=include/array_ptr.h \
       include/base64.h \
       include/cli_io_helper.h \
       include/cli_terminal_config.h \
       include/client_state.h \
       include/command_terminal_behavior.h \
       include/config_def.h \
       include/config_reader.h \
       include/cterm_ptr.h \
       include/eterm_ptr.h \
       include/exception_def.h \
       include/fd_helper.h \
       include/help_processor.h \
       include/input_terminal_behavior.h \
       include/iterminal_behavior.h \
       include/login_command_terminal_behavior.h \
       include/message.h \
       include/message_serialization.h \
       include/resource_holder.h \
       include/server_interaction.h \
       include/server_interaction_helper.h \
       include/signal_mask_holder.h \
       include/signal_safe_executer.h \
       include/signal_utils.h \
       include/socket_utils.h \
       include/string_utils.h

SOURCE=src/base64.cpp \
       src/cli_io_helper.cpp \
       src/cli_terminal.cpp \
       src/cli_terminal_config.cpp \
       src/client_state.cpp \
       src/command_terminal_behavior.cpp \
       src/config_reader.cpp \
       src/fd_helper.cpp \
       src/help_processor.cpp \
       src/input_terminal_behavior.cpp \
       src/login_command_terminal_behavior.cpp \
       src/message_serialization.cpp \
       src/server_interaction.cpp \
       src/server_interaction_helper.cpp \
       src/signal_utils.cpp \
       src/socket_utils.cpp \
       src/string_utils.cpp

EXTERNAL_INCLUDE=-I/usr/include/readline/ \
                 -Iinclude/

EXTERNAL_LIB=-L/usr/lib/x86_64-linux-gnu -lreadline -lhistory \
             -L/usr/lib/erlang/lib/erl_interface-3.7.13/lib -lerl_interface -lei \
             -lpthread

all: pre_build
	$(CC) $(CFLAGS) $(EXTERNAL_INCLUDE) -o $(BIN)/cli_terminal $(HEADER) $(SOURCE) $(EXTERNAL_LIB)

pre_build: clean
	$(shell if [ ! -d "./$(BIN)/" ]; then mkdir $(BIN); fi)

clean:
	$(shell rm -rf $(BIN)/*)
