#!/bin/bash

openocd -f stm32f407g.cfg -c "init" -c "program build/zephyr/zephyr.hex verify reset exit"
