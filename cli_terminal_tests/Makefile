CC=g++
CFLAGS=-g -std=gnu++11 -Wall
LFLAGS=

BIN=bin

HEADER=

SOURCE=

EXTERNAL_INCLUDE=-Iinclude/

EXTERNAL_LIB=-L../external_bin -static-libgcc -lgtest -lgtest_main -lgmock -lgmock_main \
             -lpthread

all: pre_build
	$(CC) $(CFLAGS) $(EXTERNAL_INCLUDE) -o $(BIN)/cli_terminal $(HEADER) $(SOURCE) $(EXTERNAL_LIB)

pre_build: clean
	$(shell if [ ! -d "./$(BIN)/" ]; then mkdir $(BIN); fi)

clean:
	$(shell rm -rf $(BIN)/*)
