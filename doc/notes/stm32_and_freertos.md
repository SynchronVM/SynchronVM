
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
