
SENSEVM_DIR = ../

include $(SENSEVM_DIR)sense-vm.mk

SOURCE_DIR = $(SENSEVM_DIR)src
INCLUDE_DIR = $(SENSEVM_DIR)include

INCLUDES = -I$(INCLUDE_DIR) -I$(SENSEVM_DIR)$(PLATFORM_INCLUDE) -I.

$(shell mkdir -p ${BUILD_DIR})

vpath %.c $(SOURCE_DIR) $(SENSEVM_DIR)$(PLATFORM_SOURCE)
SOURCES_P = $(wildcard $(SOURCE_DIR)/*.c) $(wildcard $(SENSEVM_DIR)$(PLATFORM_SOURCE)/*.c)
SOURCES = $(notdir $(SOURCES_P))
OBJECTS = $(patsubst %.c, $(BUILD_DIR)/%.o, $(SOURCES))

all: $(OBJECTS)
	$(CC) $(INCLUDES) $(CCFLAGS) $(OBJECTS) main.c -o app

debug: CCFLAGS += -g 
debug: $(OBJECTS) $(LIB)

$(LIB): $(OBJECTS) 
	$(AR) -rcs $@ $(OBJECTS)

$(BUILD_DIR)/%.o: %.c
	$(CC) $(INCLUDES) $(CCFLAGS) -c $< -o $@

clean:
	rm -f ${BUILD_DIR}/*.o
	rm -f ${BUILD_DIR}/*.a
	rm -f app



