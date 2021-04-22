# SPDX-License-Identifier: Apache-2.0

cmake_minimum_required(VERSION 3.13.1)
find_package(Zephyr HINTS $ENV{ZEPHYR_BASE})
project(lldriver-test)

set(LLBC   ../../../../src/ll)
set(LLBC_Z ../../../../src/ll/os/zephyr)

set(HAL_Z  ../../../../src/hal/zephyr)

set(LLBH   ../../../../include/ll)
set(LLBH_Z ../../../../include/ll/os/zephyr)

set(HALH_Z  ../../../../include/hal/zephyr)

set(SENSE_VM ../../../../src)       
set(SENSE_VM_H ../../../../include) 

FILE(GLOB app_sources src/*.c)
FILE(GLOB extra_sources
  ${SENSE_VM}/*.c
  ${LLBC}/ll_driver.c
  ${LLBC}/ll_led.c
  ${LLBC}/ll_uart.c
  ${LLBC_Z}/led.c
  ${LLBC_Z}/uart.c
  ${HAL_Z}/svm_zephyr.c)
#  ${LLBC_Z}/powerman.c)

target_sources(app PRIVATE ${app_sources} ${extra_sources})
target_include_directories(app PRIVATE
  .
  ${SENSE_VM_H}
  ${LLBH}
  ${LLBH_Z}
  ${HALH_Z}
  ${ZEPHYR_BASE}/kernel/include
  ${ZEPHYR_BASE}/arch/${ARCH}/include)


zephyr_library_include_directories(${ZEPHYR_BASE}/samples/bluetooth)
