
# Random STM32, FreeRTOS and Microcontroller info 


## Ons Nov 18 
 Interrupts: 
  - https://developer.arm.com/documentation/ddi0403/latest/
 
  - ICSR  : Interrupt control and state register. 
  - AIRCR : The Application Interrupt and Reset Control Register.
  
  - Higher priority interrupts can preempt lower priority interrupts. 
  - Ordering can be set in case 2 interrupts on same level are pending. 
    - (?) Not used in FreeRTOS, just the pre-emption level. (level 4). 
  	- Set higher priority on interrupts with stricter timing requirements. 
	  But then you cannot use FreeRTOS functions in that interrupt (as the interrups 
	  will pre-empt the scheduler.
	  
  - FreeRTOS
	- xQueueSend - xQueueReceive : send data between FreeRTOS tasks. 
		Sender should yield so receiver has chance to run. 
		- XQueueSend blocks if queue is full and task goes to sleep. 
	- Send data from ISR use portYIELD_FROM_ISR. sets the PendSV interrupt as pending. 
		xQueueSendFromISR. xQueueSendFromISR does not block. 
	
	- Notfication can be send to tasks or taskgroups. 
	- Semaphores. 
	
	- vTaskDelayUntil. 
   
    - Return hierarchy for interrupts, interrupting interrupts the stack maintains.
  	
	- Watchdogs, window-watchdog
	- window-watchdog trigger from lowest priority task. if watchdog is never 
	  called there is a problem. 

  
- https://github.com/GENIVI/CANdevStudio
- https://github.com/commaai/opendbc


## Mon Nov 23

- C Programming. 
  described 4 arrays => text (program code), data (initialized data), bss (uninitialized data), isr_vectors.
  
  bss is zeroed at start-up 
  
  (above is the contents of an elf-file)
	
  C-Code maps very directly to the above description with th 4 sections. 
  
- High level patterns for driver development
 - struct pattern
 - multilevel struct pattern. 
 - opaque objects
 - container of pattern
 
- struct pattern 
  - all data stored in structs, passed as arguments (not globals). 
  - stuct variable never declared global. 
  - all functions accept pointer to struct. 
  - data flow is always through the code (and not outside of it). 
  - functions never operate on global state. 
  
  Methods on structures always take a "self"-pointer.
  
  The struct pattern takes control of the data-flow through the application. 
  
  
- Hierarchical containment 
  - Related variables are grouped into structs and included into other structs.
  - Every function  takes a pointer to a struct that contains only data relevant 
    to that function.
  - multiple instances of behaviours can be created. 
  - make "local scopes" with inline structs. (grouping of concerns). 
  

- Opaque objects  (when writing platform dependent stuff) 
  - opaque objects are allocated on the heap. 
  - object definition is same file as implementation (not public object). 
  - public interface only contains methods that operate on a pointer to the opaque object. 

  
- Container of pattern 
  - Aggregate item (list struct) is embedded in an enclosing struct. 
  - pointer to item is passed around and used for organizing items. 
  - pointer to main struct is retrieved using container_of macro to get 
    the actual data. 
	
  (Polymorphism). 
  
  Implementation of Abstract interface.
  Type safe! 
 
------
- Device driver patterns
  - interrupt pattern 
  - task pattern 
  - Deferred work pattern. (interrupt finishes quickly other thread continues the work)
  - cache pattern
  - aggregator pattern 
  
- Interrupt pattern 

- Task pattern (When things happen on different heartbeats)
  - separation into subsystems with different timing requirements. 
  - processes with different timing as sequential tasks (sleep, scheduler). 
  - Scheduler is responsible for waking up tasks when events occurs. 
  - task priority gives precedence to high prio tasks over low. 
  
- Deferred work pattern.
  - seamless transition from interrupt to application
  - only possible when task pattern is used. 
  - interrupt pushes data on a queue and exists. 
  - dedicated task does the work of handling the data pushed on the queue 
  - task sleeps in wait for more data on the queue. 
  - example interrupt driven uart. 
  
- cache pattern
  - enables fast access to sensor reading by all other tasks 
  - saves time 
  - only possible with task pattern 
  - dedicates task is repeatedly reading sensor at fixed rate. 
  - values are filtered, interpolated and "cached".
  - tasks call read (gets the cached value). 
  
-- Aggregator pattern
  - collection of interfaces. 
  
----------------------------------
-- tools to look at the elf file
 - objdump
 - nm
----------------------------------

--- 
interrupt: 
  - use static global variables to communicate with the main loop. 
  - Use an accessor function that disables the interrupt while accessing the 
    shared variable. 
  - See stm32_uart.c as an example. 
  
  
## Tue Nov 24

Theme:  RTOS high resolution timing. 
  "RTOS features for enforcing timing"
  An RTOS is a means to enforce timing. 
  
* Measure time 
* Partition timing resources
* Enforce timing
* Simplify Software structure
* Solves problem for parallel workloads
* ensure portability

  "One task for each timing domain" 
  

App          |   Driver                  |     HW 
Write data   |                           |
             | Set data ptr            |                   |
             | enable tx_rdy interrupt |                   |
             | start transmission      |                   |
             |                         |                   |
             | Sleep                   | data transmission |
             |                         |                   |
             | wait more data or done  |                   |



- Timers and delays 
  - multiples of the tick counter for the RTOS
  
if you need intervals that are not a multiple of tick counter
  - Wrong way: Busy wait loop 
    - Stops counting if interrupted by interrupt. 
  - DWT: Cycle counter. increments each clock cycle 
    - 32 bit counter. overflows. 
    - Always compare it as a subtraction to handle overflow of 
	  counter. 
	- Can have a jitter in the precision.
	- essentially a busy loop. but better as it checks 
	  a counter. Do not use for long delays, only very short ones. 
    - DWT->CYCNT = 0; // reset
	- DWT->CTRL  |= 1; // enable count
	  
  - vTaskDelay: provided by the RTOS and limited to muiltiples 
    of tick counter. for example 1ms. 
	- ideal for delays of several ms - to a few seconds. 
	- puts the task to sleep. consumes less CPU. 
	- blocks the currently running task
	
	
  - Asynchronous delays
    - RTOS Timers: asynchronous timers that calls a callback. 
	  - multiples of systick. 
	  - notification on expiration. 
	  - Running as part of a special RTOS timer thread. 
	- Hardware Timers: 
	  - High precision (does not depend on systick). 
	  - combine with notification that can be sent to task. 
	  - launch timer and set task to wait for notification. 
	  - ulTaskNotifyTake() 
	  - vTaskNotifyGiveFromISR()
	  - portYIELD_FROM_ISR()
	  

- If you have two tasks on the same priority (both looping)
  They must sleep! (Yield occasionally). 
  

- xTaskGetTickCount()

- How to select what frequency to use on the systick (which triggers tasks switches) 
  - tradeoff between rate and cost of switch. 
  		 
## Wed Now 25 

DMA on the STM32 

DMA Controller is a separate little controller. 


- Automatic transfer between memory locations.
- Peripheral to memory
- memory to peripheral
- memory to memory
- can be configured
  - increment src, dst or both pointers 
  - if you transfer from application memory to peripheral 
    increment only src pointer. 
- dedicated streams 
- High data rates.
- Interrupts at half transfer or full transfer. 

- Use DMA to respond to the arrival of data instantaneously 
  - no software involved in the transfer. 
  
- Uart can be configured to use DMA
  - configure uart to generate request 
  - configure DMA controller to respond to this request. 
  
- DMA can be used to send data directly to GPIO pins 
  - bit-banging! 
- DMA can be used to implement more complex protocols 
  that require very precise timing. 
  
DMA Streams: 
- Each DMA stream connected to a few different peripherals. 
- A stream can only be used for one thing at a time. 
- See STM32F4 reference manual (or datasheet).  Table 42, 43

- There are priority levels. 
  
DMA Modes: 
- What happens when transfer completed
- NORMAL mode: 
  - single transfer 
  - DMA disables after transfer. 
- CIRCULAR mode: 
  - Start transfer
  - When transfer finishes, DMA is restarted from the start of the buffer.
  - Ideal for reception of data. 
  - Will overwrite oldest data. 
  - Stream transmission or stream reception without interruptions. 
  - Interrupt can be set at half-buffer. 
- DOUBLE BUFFER mode:
  - Software can update memory register while transfer is active. 
  - buffers swapped at end of each transaction. 

DMA PRIORITIES: 
- very high
- high
- medium
- low. 

DMA Restrictions: 
- You cannot use all memory (not the core coupled memory) 
- there is a max transfer size. 
- Very expensive if you transfer small amounts of data.

DMA Events: 
- Half transfer event. half buffer transferred. 
- Transfer complete. 
- There is one interrupt per stream. 

When using DMA with a Peripheral your application will not be listening 
for peripheral events anymore. Rather it will just wait for DMA events (interrupts). 


DMA Fifos: 
- You can fifo to bridge between different data-widths. 


Example UART: 
- Transfers happen entirely in hardware. 
- Optimal for highest baud rate and low power systems. 
- optimal for sending packets for fast communications
-* How do we know that a reception is actually done? 
   - Idle line detection (definitely exists on F4 and above). Configure 
     the UART to use idle line detection. 
   - receive timeout

- Problems with UART without DMA 
  - Depends heavily on fast interrupt handling
  - an interrupt for every single byte
  - high probability of missing bytes. 
  
UART DMA TX:
- Configure DMA to transfer blocks of a given size. 
- Uart is configured as usual. 
- Enable DMA2_Stream7 interrupt in NVIC (DMA interrupt) (UART 1 uses stream 7).
- Enable TX DMA request on UART.


UART DMA RX: 
- Configure uart as usual 
- Enable idle interrupt
- set transfer length
- set memory and peripheral addresses
- set direction 
- set mode (circular or normal)
- enbla DMA and UART
- application is notified for each half of the buffer received so you ca
  copy the data to you application "space".
- application is notified by IDLE event when reception is completely 
  ended. 


Random stuff
- LOOK AT the bus matrix! 
  - DMA is at most "as fast as" a memcpy. 
  - parallelism between the streams.
  
- Peripherals can generate RX or TX requests to the DMA controller. 


- Data burst: a mode that exists in the STM32. 
  - Single transfer of 4, 8 or 16 beats.
  - 10.3.11 in reference manual RM0090 REV 18
  
  
  
- DWT counter use this to measure number of cycles between 2 points.


## Mon Nov 30 

A look at some projects to see what patterns they use. 

Looking at the vesc BLDC code. 
 - Does not consistently follow the design patterns from earlier. 
 - Argument placement not consistent. 
 
 
## Tue Dec 1

STM32 Timers! 

- Suitable for multichannel PWM generation 
- multichannel input capture
- start upon a trigger event 
- control external hardware 
- trigger output 

STM32F4: 
 - Use cases for timers: 
   - measuring pulse width
   - ouput pwm
   - read and interpret hall sensors
   - read motor rotary encoder
 - Two advanced timers TIM1 and TIM8
 - medium TIM 2,3, 4, 5
 - Simple TIM 9, 12
 - Simplest TIM 10, 11, 13, 14
 - special: DWT, SysTick 
   - DWT is the debug unit in the chip, 
     but it also has a timer. 

Advanced controll timer (TIM 1, 8) 
 - 16 bit
 - clock div 1,2,4
 - prescaler 1 to 65536
 - direction selection 
 - one pulse mode 
 - high/low output for H-Bridge
 - hall sensor input support
 - filtering 
 - repeat counter 
 - emergency brake : reset into known state. 
 - slave mode update generation 
 - Each have 4 channels. 
 
you can start ADC conversion from a timer. Trigger output. 

Timer Configuration: 
 - enable the clock for the timer (RCC) 
 - TIM_TimeBaseInitTypeDef (stdperipheral library st)
 - set mode, prescale, period, division. 
 - very flexible selection of period, by division and prescaler. 

Clock Selection: 
 - Internal clock (CLK_INT) from RCC. 
 - External clock from pin 
 - External clock from trigger input 
 - internal tridder inputs (ITRx).
 

Output compare: 
 - Generate waveform by comparing a value to the current counter value
   OCxM register compared to coutner. 
 - PWM 1 & 2 (pos, neg)
 - Center aligned or edge aligned 
 - When switchin transistors current draw increases 
   - center aligned spreads out the switching pulses. 
   
Input Capture: 
 - Measure pulse width 
 - measure PWM width and duty cycle 
 - capture HALL sensor 
 
HALL sensor interface in hardware: 
 - Generate events on every transition of the hal sensor. 
 
Encoder Interface: 
 - Table 93 ref manual 
 
Synchronization: 
 - A master timer can start, stop, reset or clock another 
   timer. 
 - ADC can be set to respond to timer trigger

3-phase PWM
 - Start timer clock
 - configure timebase and pwm 
 - configure output compare 
 - configure deadtime 
 - enable update interrupt


General control timers. 
 - Tim2 to Tim5 
 

The Std peripheral library can be used together with FreeRTOS.

--- 
- 30 - 60KHz pwm drives the mosfets. 
- The commutation happens at artificial intervals. Low frequency updates
  (but several times per rotation of motor?)


Commutations of 3 phase motor 
   U     V    W 
1  +     -   off 
2  +    off   - 
3  off   +    -
4  -     +   off
5  -    off   + 
6  off   -    +


## Wed Dec 2 2020

Zephyr RTOS 
 - How Zephyr is architected. 
 - How to set up a Driver 
 - How to set up a new Board. 
 
Started as a project by windriver, now in linux foundation. 
 - Evolved tremendously! 

Device tree based configuration of embedded systems. 
 - small devices does not allow a device-tree-BLOB in memory. 
 - The blob in memory is not efficient 
 - Zephyr solves this by compiling the Device tree to C headers and code. 
   together with macro magic. 
 - Static instantiation of drivers. 
 - Truely really eliminate dynamic memory allocation. 
 
Project Structure: 

The west tool creates a lot for you 
 - bootloader 
 - modules
 - tools 
 - zephyr 
 - app 
   - boards 
   - drivers 
   - dts 
   - include 
   - src

The app has a predefined directory structure so zephyr knows where 
everything is. 

 - Zephyr supports memory protection on embedded platforms
   - kernel space and user space 
   - that is why drivers are separate. 
   
Board Directory: 
 - boars/<arch>/<board>/yourboard
   - .dts
   - _overlay 
   - board.cmake
   - _defconfig
   - .yaml             : definition for west tool 
   - Kconfig.board
   - Kconfig_defconfig

make menuconfig starts graphical config editor. 


Start a new Application: 

 - west init <directory> 
 - cd directory 
 - west update 
 - source zephyr-env.sh
 - mkdir <app-directory>
 - cd <app-directory> 
 - create CMakeLists.txt
 - create src/main.c
 - mkdir build
 - cd build
 - cmake -DBOARD=strm32f429i-disc1
 - make
 - make flash
 
Device Tree Overlays 
 
 - zephyr creates bindings dts/bindings for access to the peripherals 
   (pins) and settings of the device tree. 
 - Describe drivers
 - DEVICE_AND_API_INIT - see Zephyr documentation 
 
API Definitions 
 - include/drivers 
 - Abstracts devices between platforms 
 
PIN MUX 
 - Flexible way to define pins 
 - System for configure the alternative functions of the pins 
  
BACK TO BOARDS 

 Board OpenOCD config 
  - may need to create this yourself 
  - default configuration may not work if you do not use reset pin for example. 
  - openocd.cfg
  
 Board CMake
  - add to boards folder. 
  - look at a default one and copy
  
 Board DTS 
  - copy an existing  (or include files)
  - define all devices 
  - "Compatible" string can be used to write a board specific driver. 
  - set console to UART1 for example. 
  - status = OK : means the device is going to be used. 
  - only device tree nodes with status OK will be instantiated. 
  
 Board Overlay 
  - You have a supported board but want to change some details use an overlay. 
  - Define a board variant. 
  
 Board Defconfig
  - Look at existing defconfigs to get a good feeling for how these work. 
  - Clock configuration 
  - MPU enable 
  - Stack protection 
  - serial console, uart console. 
  - Set the HSE clock (8MHz, 16MHz)
  - set the PLL values (configure the derived clocks). 

 Board YAML
  - Just lets west know that this board exists. 
  
 Board KConfic
  - Hierarchical menu structure 
  
 Board defaults 
  - defaults for the menu config. 
  
 Menuconfig 
  
 Zephyr vs FreeRTOS 
  
  FreeRTOS 5 - 6 files <-> Zephyr 19333 files 
  
  Zephyr:
   - Has data-structures. 
   - APIs. 
   - write quite generic code.
   - Scheduler
   - separates settings from code
  
  FreeRTOS:
   - Scheduler. 
   
## Fri Dec 4 

RTOS and STM32  (Zephyr)


FMC SDRAM Driver for STM32F4 in Zephyr.


Drivers/memc/Kconfig.stm32 

 - FMC CLOCK is on AHB3
 - clock control is configured in clock_stm32_ll_common.c 
 - we need to add support for AHB3 clock. 
 - Datasheet gives wich bit to set.  (READ THE DATASHEETS)
   RCC_AHB3ENR
 - Add a  device tree node to enable this bit in the FMC device tree node. 
  
SDRAM
 - SDRAM for example on stm32F429i-disc1
 - SDRAM 
   - Ax  : address wires
   - Dx  : data wires
   - BAx : bank
   - Clk
   - cke
   - cs 
   - ras
   - cas
   - wr
   - LDQM UDQM: lower/upper byte mask

Configure the device tree. 
  - Look at the Schematic (how is the memory connected to the mcu). 
  - for every pin, create a pinmux-0 entry. 
  - if any pins are missing you may need to add them at SOC level 
    - middle level device tree files. (not the lowest level)
	
  - Pins are named in the device tree as they are named in the datasheet. 
  
  - The board dts should just be doing tweaks to things already defined in lower 
    level dts files that describe the SOC. 
	
  - look at example code from ST on how information for how to configure the 
    driver. 
	- Look at the datasheet for the SDRAM chip used. 
	
  Add an SDRAM node 
   - sdram:2 sdram@d0000000 
     - provide address and size.
   - dts file node. (where leds are defined) 
   
	
	
	



 
