CC=g++
CFLAGS=-g -std=gnu++11 -Wall
LFLAGS=

BIN=bin

#SOURCE=src/terminal_work_example_1.cpp

#OBJECTS=$(SOURCE:%.cpp=%.o)

#%.o: %.cpp
#	$(CC) $(CFLAGS) $< -o $@

all: pre_build
	$(CC) $(CFLAGS) -o $(BIN)/terminal_work_example_1 src/terminal_work_example_1.cpp
	$(CC) $(CFLAGS) -I/usr/include/readline/ -o $(BIN)/terminal_work_example_2 src/terminal_work_example_2.cpp -L/usr/lib/x86_64-linux-gnu -lreadline -lhistory
	erlc -Werror -b beam -o $(BIN) src/terminal_work_example_heavy_command.erl
	erlc -Werror -b beam -o $(BIN) src/terminal_work_example_light_command.erl
	erlc -Werror -b beam -o $(BIN) src/terminal_work_example_server.erl
	$(CC) $(CFLAGS) -I/usr/include/readline/ -o $(BIN)/terminal_work_example_client src/terminal_work_example_client.cpp\
                                             -L/usr/lib/x86_64-linux-gnu -lreadline -lhistory\
                                             -L/usr/lib/erlang/lib/erl_interface-3.7.13/lib -lerl_interface -lei\
                                             -lpthread
	$(CC) $(CFLAGS) -I/usr/include/readline/ -o $(BIN)/readline_example src/readline_example.cpp -L/usr/lib/x86_64-linux-gnu -lreadline -lhistory

pre_build: clean
	$(shell if [ ! -d "./$(BIN)/" ]; then mkdir $(BIN); fi)

clean:
	$(shell rm -rf $(BIN)/*)
