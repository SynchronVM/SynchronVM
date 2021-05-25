
# Tests

SENSEVM_DIR = ../../

include $(SENSEVM_DIR)sense-vm.mk

SOURCE_DIR = $(SENSEVM_DIR)/src
# INCLUDE_DIR = $(SENSEVM_DIR)/include

# INCLUDES = -I$(INCLUDE_DIR) \
#            -I$(SENSEVM_DIR)$(PLATFORM_INCLUDE) \
#            -I$(INCLUDE_DIR)/ll \
#            -I.

$(shell mkdir -p ${BUILD_DIR})

SVM_SOURCES = $(SOURCE_DIR)/CAM.c \
              $(SOURCE_DIR)/channel.c \
              $(SOURCE_DIR)/chan_recv_queue.c \
              $(SOURCE_DIR)/chan_send_queue.c \
              $(SOURCE_DIR)/event.c \
              $(SOURCE_DIR)/heap.c \
              $(SOURCE_DIR)/queue.c \
              $(SOURCE_DIR)/RTS.c \
              $(SOURCE_DIR)/scheduler.c \
              $(SOURCE_DIR)/stack.c \
              $(SOURCE_DIR)/VMC.c \
              $(SOURCE_DIR)/ll/ll_driver.c

TEST_SOURCES = $(wildcard *.c)
TEST_EXECS = $(patsubst %.c, %.exe, $(TEST_SOURCES))

TEST_INCLUDES = -I$(SENSEVM_DIR)/include -I.

all: $(TEST_EXECS) 

debug: CCFLAGS += -DDEBUG -g
debug: $(TEST_EXECS) $(OBJECTS)

%.exe: %.c $(SVM_SOURCES)
	$(CC) $(TEST_INCLUDES) $(CCFLAGS) $(SVM_SOURCES) $< -o $@ 

clean:
	rm -f $(BUILD_DIR)/*.o
	rm -f *.exe
	rm -Rf *.exe.dSYM

