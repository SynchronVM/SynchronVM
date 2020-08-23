

SENSEVM_DIR = ../
SENSEVM_LIB_DIR = $(SENSEVM_DIR)/build/linux-x86

INCLUDES = -I../include -I.


all: $(SENSEVM_LIB_DIR)/libsensevm.a main.c
	$(CC) $(INCLUDES) $(CCFLAGS) main.c $(SENSEVM_LIB_DIR)/libsensevm.a -o app

clean:
	rm app
