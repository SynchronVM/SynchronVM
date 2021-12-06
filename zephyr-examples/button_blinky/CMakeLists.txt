# SPDX-License-Identifier: Apache-2.0

cmake_minimum_required(VERSION 3.13.1)
find_package(Zephyr HINTS $ENV{ZEPHYR_BASE})
project(lldriver-test)

## set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -DDEBUG")

set(LLBC   ../../backend/src/ll)

set(SENSE_VM ../../backend/src)       
set(SENSE_VM_H ../../backend/include)

set(PLATFORM_SRC ../../backend/platform/zephyr/src)
set(PLATFORM_H   ../../backend/platform/zephyr/include)

FILE(GLOB app_sources src/*.c)
FILE(GLOB extra_sources
  ${SENSE_VM}/*.c
  ${LLBC}/ll_driver.c
  ${PLATFORM_SRC}/led.c
  ${PLATFORM_SRC}/uart.c
  ${PLATFORM_SRC}/button.c
  ${PLATFORM_SRC}/sys_time.c
  ${PLATFORM_SRC}/sys_debug_uart.c
  ${PLATFORM_SRC}/svm_zephyr.c
  ${PLATFORM_SRC}/usb_cdc.c)
#  ${LLBC_Z}/powerman.c)

target_sources(app PRIVATE ${app_sources} ${extra_sources})
target_include_directories(app PRIVATE
  .
  ${SENSE_VM_H}
  ${SENSE_VM_H}/sys
  ${PLATFORM_H}
  ${ZEPHYR_BASE}/kernel/include
  ${ZEPHYR_BASE}/arch/${ARCH}/include)


zephyr_library_include_directories(${ZEPHYR_BASE}/samples/bluetooth)
