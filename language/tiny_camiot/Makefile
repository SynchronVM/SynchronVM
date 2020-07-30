

BUILD_DIR = build/linux-x86
CCFLAGS = -g -O2 -Wall -Wextra -pedantic -std=c11
CC=gcc

SOURCE_DIR = src
INCLUDES = -Iinclude -Igrammar -I../../include

$(shell mkdir -p ${BUILD_DIR})

SRC = src
OBJ = obj

SOURCES = $(wildcard $(SOURCE_DIR)/*.c)
OBJECTS = $(patsubst $(SOURCE_DIR)/%.c, $(BUILD_DIR)/%.o, $(SOURCES))

PARSER  = grammar/Absyn.o grammar/Lexer.o grammar/Parser.o grammar/Printer.o

all: camiotc 

$(BUILD_DIR)/%.o: $(SOURCE_DIR)/%.c 
	$(CC) $(INCLUDES) $(CCFLAGS) -c $< -o $@

camiotc: $(OBJECTS) $(PARSER)
	$(CC) -o $@  $(CFLAGS) $(OBJECTS) $(PARSER)

$(PARSER):
	cd grammar; ./make.sh

clean:
	rm -f ${BUILD_DIR}/*.o
	rm -f ${BUILD_DIR}/*.a
	cd grammar; make clean

