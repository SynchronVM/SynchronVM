
# Tests

SENSEVM_DIR = ../

include $(SENSEVM_DIR)sense-vm.mk

SOURCE_DIR = $(SENSEVM_DIR)/src
INCLUDE_DIR = $(SENSEVM_DIR)/include

INCLUDES = -I$(INCLUDE_DIR) -I$(SENSEVM_DIR)$(PLATFORM_INCLUDE) -I.

$(shell mkdir -p ${BUILD_DIR})


vpath %.c $(SOURCE_DIR) $(SENSEVM_DIR)$(PLATFORM_SOURCE)
SOURCES_P = $(wildcard $(SOURCE_DIR)/*.c) $(wildcard $(SENSEVM_DIR)$(PLATFORM_SOURCE)/*.c)
SOURCES = $(notdir $(SOURCES_P))
OBJECTS = $(patsubst %.c, $(BUILD_DIR)/%.o, $(SOURCES))

$(info $(SOURCES_P))
$(info $(SOURCES))
$(info $(OBJECTS))

TEST_SOURCES = $(wildcard *.c)
TEST_EXECS = $(patsubst %.c, %.exe, $(TEST_SOURCES))

TEST_INCLUDES = -I$(SENSEVM_DIR)/include -I.

all: $(TEST_EXECS) $(OBJECTS)

%.exe: %.c $(OBJECTS)
	$(CC) $(TEST_INCLUDES) $(CCFLAGS) $(OBJECTS) $< -o $@ 

$(BUILD_DIR)/%.o: %.c
	$(CC) $(INCLUDES) $(CCFLAGS) -c $< -o $@

clean:
	rm *.exe

