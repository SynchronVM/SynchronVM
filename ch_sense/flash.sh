#!/bin/sh
openocd -f stm32f407g.cfg -c "program ./build/SENSEVM_TEST.elf verify reset exit"
