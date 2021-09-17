# SPDX-License-Identifier: Apache-2.0

cmake_minimum_required(VERSION 3.20.0)
find_package(Zephyr HINTS $ENV{ZEPHYR_BASE})
project(scanner)

FILE(GLOB app_sources src/*.c)

# FILE(GLOB extra_sources
#   ../common/src/*.c
#   ../common/src/abstraction/zephyr/*.c
#   ./peng/runtime/src/*.c
#   )

  # ../common/src
  # ../common/src/abstraction/zephyr
  # ./peng/runtime/include
  # ./peng-platform/nrf52/include


target_sources(app PRIVATE ${app_sources} ${extra_sources})
target_include_directories(app PRIVATE
  ${ZEPHYR_BASE}/kernel/include
  ${ZEPHYR_BASE}/arch/${ARCH}/include)

