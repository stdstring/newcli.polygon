CC=g++
CFLAGS=-g -std=gnu++11 -Wall
LFLAGS=

BIN=bin

#SOURCE=src/terminal_work_example_1.cpp

#OBJECTS=$(SOURCE:%.cpp=%.o)

#%.o: %.cpp
#	$(CC) $(CFLAGS) $< -o $@

all: pre_build $(SOURCE)
	$(CC) $(CFLAGS) -o $(BIN)/terminal_work_example_1 src/terminal_work_example_1.cpp

pre_build: clean
	$(shell if [ ! -d "./$(BIN)/" ]; then mkdir $(BIN); fi)

clean:
	$(shell rm -rf $(BIN)/*)

