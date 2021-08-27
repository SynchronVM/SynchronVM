# Sense-VM (Work in progress)

Sense-VM is a bytecode-vm for microcontrollers such as STM32, and
NRF52. Sense-VM is based on the Categorical Abstract Machine but is augmented
to support Concurrent ML style concurrency.

## MPLR 2021 paper snapshot

[V0.1.0](https://github.com/svenssonjoel/Sense-VM/releases/tag/v0.1.0)


## Building the compiler

The compiler is implemented in Haskell and uses the stack build
system ([get stack](https://docs.haskellstack.org/en/stable/README/)).
Go to the `frontend/CamIoT` directory and issue the command:

```
stack build
```

The command above builds an executable called `camiotc` somewhere
under the `.stack-work` directory that the `stack` tool generates.

To install the binary to `$HOME/.local/bin` issue the command:

```
stack install
```

## Using the camiotc compiler

`camiotc` takes one or more arguments one of which should be a source file.
For example, to compile one of the example programs under `testcases` do:

```
camiotc testcases/good11.cam 
```

The output of this command should be:

```
compiling file `testcases/good11.cam` to output `out.svm`
```

Alternatively a specific output file can be specified using the `-o` parameter:

```
camiotc testcases/good11.cam -o good11.svm
```

and the output should be:

```
compiling file `testcases/good11.cam` to output `good11.svm`
```

# Examples 

Examples are located in the `examples` directory and are set up to
build together with ChibiOS 20.3.3. Your copy of the Chibios source
code should be located at position `../../../ChibiOS_20.3.3` relative
to a given example directory. 

## The `button_blinky` example

The `button_blinky` example consists of the following files: 

```
.
├── button_blinky.cam
├── Makefile
├── src-chibios
│   ├── chconf.h
│   ├── flash.sh
│   ├── halconf.h
│   ├── Makefile
│   ├── mcuconf.h
│   └── stm32f407g.cfg
├── svm_chibios_conf.h
└── vm-conf.h
```

 - `button_blinky.cam` is the source code of the example program.
 - `Makefile` compiles the example to bytecode and compiles the
   virtual machine with the specifications provided in `svm_chibios_conf.h` and `vm-conf.h`
 - The `src-chibios` directory contains files related to the configuration of ChibiOS.
 - `svm_chibios_conf.h` contains information related to peripherals on the target board. 
 - `vm-conf` contains specification of virtual machine parameters. 

The `svm_chibios_conf.h` contains microcontroller and board specific
mappings of hardware (pins/peripherals) to concepts known to SenseVM. 

``` C
#ifndef SVM_CHIBIOS_CONF_H_
#define SVM_CHIBIOS_CONF_H_

#define LED0_GPIO GPIOD
#define LED0_PIN  13
#define LED0_MODE PAL_MODE_OUTPUT_PUSHPULL | PAL_STM32_OSPEED_HIGHEST

#define LED1_GPIO GPIOD
#define LED1_PIN  12
#define LED1_MODE PAL_MODE_OUTPUT_PUSHPULL | PAL_STM32_OSPEED_HIGHEST

#define BUTTON0_GPIO GPIOA
#define BUTTON0_PIN  0
#define BUTTON0_MODE PAL_MODE_INPUT
#define BUTTON0_EVENT_MODE PAL_EVENT_MODE_BOTH_EDGES

#define SYS_TIMER  5 /*select tim5 for system time*/

#endif 
```

`vm-conf` contains configuration of the virtual machine. Below is an excerpt from this file.

``` C
#define VMC_CONTAINER_1_HEAP_SIZE_BYTES       2048
#define VMC_CONTAINER_1_BYTECODE_FILE         "button_blinky.svm"
#define VMC_CONTAINER_1_STACK_SIZE_BYTES      1024
#define VMC_CONTAINER_1_ARRAY_MEM_SIZE_BYTES  0

#define VMC_CONTAINER_1_USE_BUTTON_0          1
#define VMC_CONTAINER_1_USE_LED_0             1
```

To build the example run `make` in the example directory. This relies
on ChibiOS 20.3.3 being in the right location and that you have cross
compilation tools installed ([Get'em
here](https://developer.arm.com/tools-and-software/open-source-software/developer-tools/gnu-toolchain/gnu-rm/downloads)). It
also requires that you have built and "installed" the camiotc compiler
using the instructions above.

The examples are set up to run on STM32F4-Discovery (the stm32f407 model) and
to be flashed using [OpenOCD](https://openocd.org/). To flash a Discovery board, 
connect the programming interface USB cable and run `make flash`.

After flashing, the program will immediately start running on the board. Currently some 
debug information will be printed over the USB interface (the USB port on the oposite end 
to the programming interface on the discovery board).

### What about Zephyr??!?!

SenseVM can be built on top of ChibiOS or ZephyrOS, currently the
"user friendly" examples are just set up to use ChibiOS. There is no
real reason for this other than it is what we had time to do first. 

Examples on top of ZephyrOS will appear when time allows. 

# About

First commit 8 July 2020.

## Papers on SenseVM

[Higher-Order Concurrency for Microcontroller](https://abhiroop.github.io/pubs/sensevm_mplr.pdf) - MPLR '21
