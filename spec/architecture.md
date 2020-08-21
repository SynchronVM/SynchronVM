
# Sense-VM architecture 


## Single Threaded VM 


## Concurrent VM 

```plantuml
node "Runtime Support"  as rs { 
	frame "Hardware Abstraction Layer" {
		[Drivers]
		[Interrupt routines]
		[Communication]
		[Timers]
	}

	rectangle "Virtual Machine Container 0" as vmc0
	rectangle "Virtual Machine Container N ?" as vmcn
	
	vmc0 -[hidden]-> vmcn
	
	
	frame "Power Management" as pm { 
		[Power Monitor] 
		[Sleep Manager] 
	}
	
	frame "Container Scheduler" as cs { 
		rectangle "Ensure higher priority container\ncan execute when needed"
	}
	
	pm -[hidden]-> cs
}
 
```

**Hardware Abstraction Layer**

The hardware abstraction layer (HAL) will probably be implemented by building upon an existing HAL system such as ChibiOS, ContikiOs or ZephyrOS.
Other HALs could also be candidates: CMSIS, HAL from ST for STM32 platforms for example. We should try to keep as much of the code as possible portable 
so that it can be ported to HALs that parhaps have more desirable features. 
It will be a collection of functions (and perhaps routines running on a timer interrupt periodically if needed) that can be called 
from the runtime support system.

**Sleep Manager** 

If there is no processing going on (reported from the various schedulers) the sleep manager puts the system to sleep for a period of time such that 
it wakes up when it is time for the context/container with the earliest "wake-up" time to start executing.

The sleep manager may also be prompted to wake the system up by for example a sensor driver when the sensor has new samples to process. 

It is not unthinkable that there are applications that will just sleep indefinitely unless there is outside stimulus. It may still be 
desirable that these applications wake up periodically and just let the monitoring system know that it is still alive and ok (a heartbeat).

**Container Scheduler and Virtual Machine Containers**

A virtual machine container should be an isolated executing environment for a virtual machine. 
(TODO: Should there really be more than one container? )

The container scheduler ensures that each container gets a time slot to execute. 
Possibly, these containers could be implemented using a "thread" feature of an underlying OS/RTOS/HAL or be given a certain number of iterations 
of some main loop. 

Containers should be isolated from eachother to allow different level of criticality to different containers. High criticality containers should 
be prioritised over low criticality containers.
 

```plantuml
node "Virtual Machine Container" {
	
   	rectangle "Memory" as mem {
		rectangle "Flash" {
			rectangle "Constants"
			rectangle "Code"
		}
		rectangle "RWM" {
			rectangle "Heap" 
			rectangle "Stack"
			rectangle "Array storage"
		}
	}
	rectangle "Virtual Machine" as vm
		
	rectangle "Memory Management\nRuntime Support" as mm
	
	vm -> mm 
	mm -> mem

	caption A VM container consist of a virtual machine instance and a set of dedicated and private memory resources.

}
``` 

```plantuml
node "Virtual Machine" as vm { 
	frame "Registers" { 
		[PC] 
	    [SP] 
		[CP]
		[EP]
		
	}
	
	[Context List]
	[Execution Unit]
	
	Frame "Manager" {
	[Context Scheduler]
	[Progress Monitor]
	}
} 
```
**Virtual Machine** 

The virtual machine is based upon the Categorical Abstract Machine (CAM) and consists of number of registers and 
and an execution unit. (More details later)

**Execution Unit** 
The execution unit reads instructions from code memory at location stored in PC register and evaluates each instruction 
over the state. 

**Context Scheduler and Context List**

There is a list of contexts (separate instances of code and state) that can execute in a time-shared fashion on the 
execution unit. 

(TODO: Maybe cooperative scheduling at this level? This would require "go to sleep" operations in the bytecode.)

The contexts that are executed on one VM are sharing the memory resources of the Virtual Machine Container. 



<!-- [Scheduler] -> [Execution Parameters] -->
<!-- [Scheduler] -> [Sleep Manager]  -->

**Context Switching**

```plantuml

	frame "Registers" { 
		[PC] 
	    [SP] 
		[CP]
		[EP]
		
	}
	
	[Scheduler]

	node "Execution Context to Activate" { 
		[Code Memory] 
		[Stack]
		[PC-RESUME]
		[Environment]
		[Execution Parameters]
	}

    [PC-RESUME] --> [PC] : Copy
	[Stack] --> [SP] : Copy
	[Environment] --> [EP] : Copy
	[Code Memory] --> [CP] : Store pointer to
	[Execution Parameters] --> [Scheduler] : Load
	
``` 

The context list consists of some number of "Execution Context to Activate". A context is activated by copying the stored register state 
into the VM. Putting a context to sleep works in the reversed way.



<!-- node "Virtual Machine" { -->
<!-- 	[Registers] -->
<!-- 	[Stack] -->
	
<!-- 	[Execution engine] -->
	
<!-- 	[Linear Code Memory] -->
<!-- 	[Heap] -->
<!-- } -->


<!-- node "Execution context" {  -->
<!-- 	[Registers Storage] -->
<!-- 	[Stack Storage]  -->
<!-- 	[Code] -->
<!-- } -->


<!--  [Registers] -\-> [Registers Storage]  -->
<!--  [Stack] -\-> [Stack Storage] -->
<!--  [Code] -\-> [Linear Code Memory] -->
 


## Thoughts 

Splitting concurrency up between internal to VM container via contexts and external between containers open 
up to running VM containers on different cores in parallel. 

Management of other resources is also important. Communication interfaces, sensors etc. I think it would 
be beneficial to assign such resources to a VM Container and never allow the same interface to be connected to 
more than one VM Container. 
