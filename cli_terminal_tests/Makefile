CC=g++
CFLAGS=-g -std=gnu++11 -Wall
LFLAGS=

BIN=bin

HEADER=

SOURCE=../cli_terminal/src/base64.cpp \
       src/base64_tests.cpp \
       ../cli_terminal/src/config_reader.cpp \
       src/config_reader_tests.cpp \
       ../cli_terminal/src/string_utils.cpp \
       src/string_utils_tests.cpp

EXTERNAL_INCLUDE=-I/usr/include/gtest/ \
                 -I/usr/include/gmock/ \
                 -I../cli_terminal/include/ \
                 -Iinclude/

EXTERNAL_LIB=-L/usr/local/lib/gtest -lgtest -lgtest_main \
             -L/usr/local/lib/gmock -lgmock -lgmock_main \
             -lpthread

all: pre_build
	$(CC) $(CFLAGS) $(EXTERNAL_INCLUDE) -o $(BIN)/cli_terminal_tests $(HEADER) $(SOURCE) $(EXTERNAL_LIB)

pre_build: clean
	$(shell if [ ! -d "./$(BIN)/" ]; then mkdir $(BIN); fi)

test: all
	$(BIN)/cli_terminal_tests

clean:
	$(shell rm -rf $(BIN)/*)
