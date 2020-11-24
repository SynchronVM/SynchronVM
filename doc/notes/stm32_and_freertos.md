
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
  


  
		 


