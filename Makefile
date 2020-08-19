
ifndef PLATFORM
  BUILD_DIR = build/linux-x86
  CCFLAGS =  -O2 -Wall -Wextra -pedantic -std=c11
  CC=gcc
  AR=ar
  PLATFORM_INCLUDE = platform/linux-x86/include
  PLATFORM_SOURCE  = platform/linux-x86/src
else
  CC=${CROSS_COMPILE}gcc
  AR=${CROSS_COMPILE}ar
endif

ifeq ($(PLATFORM), zynq)
  CROSS_COMPILE = arm-none-eabi-
  BUILD_DIR = build/zynq
  CCFLAGS = -mcpu=cortex-a9 -mfpu=vfpv3 -mfloat-abi=hard -O2 -Wall -Wextra -pedantic
  PLATFORM_INCLUDE = platform/zync-7000/include
  PLATFORM_SOURCE  = platform/zync-7000/src
endif

ifeq ($(PLATFORM), stm32f4)
  CROSS_COMPILE = arm-none-eabi-
  BUILD_DIR = build/stm32f4
  CCFLAGS = -mcpu=cortex-m4 -mthumb -mfloat-abi=hard -mfpu=fpv4-sp-d16 -O2 -Wall -Wextra -pedantic
#-fmessage-length=0 -ffunction-sections -c -MMD -MP
  PLATFORM_INCLUDE = platform/stm32f4/include
  PLATFORM_SOURCE  = platform/stm32f4/src
endif

ifeq ($(PLATFORM), nrf52840)
  CROSS_COMPILE = arm-none-eabi-
  BUILD_DIR = build/nrf52840
  CCFLAGS =  -mcpu=cortex-m4  -mthumb -ffunction-sections -fdata-sections -mabi=aapcs -march=armv7e-m -O2 -Wall -Wextra -pedantic
  PLATFORM_INCLUDE = platform/nrf52840/include
  PLATFORM_SOURCE  = platform/nrf52840/src
endif

ifeq ($(PLATFORM), pi)
  CROSS_COMPILE = aarch64
  BUILD_DIR = /build/pi
  CCFLAGS = -O2 -Wall -Wextra -pedantic -std=c11
  PLATFORM_INCLUDE = platform/pi/include
  PLATFORM_SOURCE  = platform/pi/src
endif

SOURCE_DIR = src
INCLUDE_DIR = include


INCLUDES = -I$(INCLUDE_DIR) -I$(PLATFORM_INCLUDE)

$(shell mkdir -p ${BUILD_DIR})

SRC = src
OBJ = obj

vpath %.c $(SOURCE_DIR) $(PLATFORM_SOURCE)
SOURCES_P = $(wildcard $(SOURCE_DIR)/*.c) $(wildcard $(PLATFORM_SOURCE)/*.c)
SOURCES = $(notdir $(SOURCES_P))
OBJECTS = $(patsubst %.c, $(BUILD_DIR)/%.o, $(SOURCES))


LIB = $(BUILD_DIR)/libsensevm.a
all: $(OBJECTS) $(LIB)

debug: CCFLAGS += -g 
debug: $(OBJECTS) $(LIB)

$(LIB): $(OBJECTS) 
	$(AR) -rcs $@ $(OBJECTS)

$(BUILD_DIR)/%.o: $(SOURCES) 
	$(CC) $(INCLUDES) $(CCFLAGS) -c $< -o $@

clean:
	rm -f ${BUILD_DIR}/*.o
	rm -f ${BUILD_DIR}/*.a

