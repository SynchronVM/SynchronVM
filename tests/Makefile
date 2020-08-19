
# Tests


CCFLAGS = -g -O2 -Wall -Wconversion -pedantic -std=c11 
CC=gcc

SRC = src
OBJ = obj

SOURCES = $(wildcard *.c)
EXECS = $(patsubst %.c, %.exe, $(SOURCES))

INCLUDES = -I../include -I../platform/linux-x86/include

all: $(EXECS)

%.exe: %.c
	$(CC) $(INCLUDES) $(CCFLAGS) $< ../build/linux-x86/libsensevm.a -o $@ 


clean:
	rm *.exe

