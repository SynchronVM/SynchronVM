
# Tests

SENSEVM_DIR = ../../

include $(SENSEVM_DIR)sense-vm.mk

SOURCE_DIR = $(SENSEVM_DIR)/src
# INCLUDE_DIR = $(SENSEVM_DIR)/include

# INCLUDES = -I$(INCLUDE_DIR) \
#            -I$(SENSEVM_DIR)$(PLATFORM_INCLUDE) \
#            -I$(INCLUDE_DIR)/ll \
#            -I.

PLATFORM_SRC = $(SENSEVM_DIR)/platform/linux-x86/src
PLATFORM_H   = $(SENSEVM_DIR)/platform/linux-x86/include

$(shell mkdir -p ${BUILD_DIR})

SVM_SOURCES = $(SOURCE_DIR)/CAM.c \
              $(SOURCE_DIR)/channel.c \
              $(SOURCE_DIR)/chan_recv_queue.c \
              $(SOURCE_DIR)/chan_send_queue.c \
              $(SOURCE_DIR)/event.c \
              $(SOURCE_DIR)/heap.c \
              $(SOURCE_DIR)/queue.c \
              $(SOURCE_DIR)/priorityqueue.c \
              $(SOURCE_DIR)/RTS.c \
              $(SOURCE_DIR)/scheduler.c \
              $(SOURCE_DIR)/stack.c \
              $(SOURCE_DIR)/VMC.c \
              $(SOURCE_DIR)/ll/ll_driver.c \
              $(SOURCE_DIR)/trusted/Trustedscheduler.c \
	      $(SOURCE_DIR)/trusted/TrustedVMC.c \
	      $(SOURCE_DIR)/trusted/TrustedRTS.c \
              $(PLATFORM_SRC)/sys_time.c

TEST_SOURCES = $(wildcard *.c)
TEST_EXECS = $(patsubst %.c, %.exe, $(TEST_SOURCES))

TEST_INCLUDES = -I$(SENSEVM_DIR)/include -I$(SENSEVM_DIR)/include/sys -I$(SENSEVM_DIR)/platform/linux-x86/include -I.

all: $(TEST_EXECS) 

debug: CCFLAGS += -DDEBUG -g
debug: $(TEST_EXECS) $(OBJECTS)

%.exe: %.c $(SVM_SOURCES)
	$(CC) $(TEST_INCLUDES) $(CCFLAGS) $(SVM_SOURCES) $< -o $@ 

clean:
	rm -f $(BUILD_DIR)/*.o
	rm -f *.exe
	rm -Rf *.exe.dSYM

