# SPDX-License-Identifier: Apache-2.0

cmake_minimum_required(VERSION 3.13.1)
find_package(Zephyr HINTS $ENV{ZEPHYR_BASE})
project(lldriver-test)

FILE(GLOB app_sources src/*.c)
FILE(GLOB extra_sources
  ../common/src/*.c
  ../common/src/abstraction/zephyr/*.c)

target_sources(app PRIVATE ${app_sources} ${extra_sources})
target_include_directories(app PRIVATE
  ../common/src
  ../common/src/abstraction/zephyr
  ${ZEPHYR_BASE}/kernel/include
  ${ZEPHYR_BASE}/arch/${ARCH}/include)


zephyr_library_include_directories(${ZEPHYR_BASE}/samples/bluetooth)
