
# The Safe and Secure Virtual Machine - Sense-VM architecture

**WORK IN PROGRESS - DRAFT** 

Sense-VM is a virtual machine for IoT and embedded applications in
general. Sense-VM is a bytecode virtual machine (think Java-VM, not an
operating system hypervisor) that provides a base level of safety,
robustness and security.

Sense-VM targets 32/64Bit microcontroller based systems with at least
about 128Kb of Read Write Memory (RWM).

Sense-VM consists of a runtime-system for execution of compiled
linearly-stored bytecode within isolated containers. The bytecode programs
cannot mutate arbitrary memory addresses and all accesses to
underlying hardware goes via the runtime system. 

Sense-VM can concurrently run a number of isolated virtual exection
environments, called Virtual Machine Containers.  Each virtual machine
container contains the resources needed to execute one program and
consist of registers and memory that is private to that environment. A
program running within a virtual machine container could potentially
be concurrent itself, running a cooperative scheduler. A criticality
level (priorities) can be associated with each virtual machine
container. Peripherals and external hardware resources are assigned to
a single virtual machine container to rule out competition for
resources between applications of different criticality (priority).

Sense-VM is implemented in standard C for portability and static
analysers, infer (from Facebook) and scan-build (from the Clang
framework) are used to detect problematic code.

Sense-VM specifies an interface downwards towards hardware
functionality and peripherals, but implemnetation of that interface
from below is not considered a part of Sense-VM per
se. Microcontroller platforms come in very different forms so it makes
sense to leave the low-level implementation to the specific use
case. We do however provide implementations based on ChibiOS (Or
contikiOs or ST HAL, or so on..)
(TODO: Such interfaces downwards towards the hardware are not yet specified)
(TODO: No such "ref" implementation is implemented or specified)

# The Sense-VM Virtual Machine 

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
		[Sleep Manager]
	}

	frame "Low Level Security" as sec {
	      [Power Monitor]
	      [Clock Monitor]
	}	
	
	frame "Container Scheduler" as cs { 
		rectangle "Ensure higher priority container\ncan execute when needed"
	}
	
	pm -[hidden]-> sec
}

 
```

**Hardware Abstraction Layer**

The hardware abstraction layer (HAL) will probably be implemented by
building upon an existing HAL system such as ChibiOS, ContikiOs or
ZephyrOS.  Other HALs could also be candidates: CMSIS, HAL from ST for
STM32 platforms for example. We should try to keep as much of the code
as possible portable so that it can be ported to HALs that parhaps
have more desirable features.  The HAL will be a collection of
functions (and perhaps routines running on a timer interrupt
periodically if needed) that can be called from the runtime support
system.

**Sleep Manager** 

If there is no processing going on (reported from the various
schedulers) the sleep manager puts the system to sleep for a period of
time such that it wakes up when it is time for the context/container
with the earliest "wake-up" time to start executing.

The sleep manager may also be prompted to wake the system up by for
example a sensor driver when the sensor has new samples to process.

It is not unthinkable that there are applications that will just sleep
indefinitely unless there is outside stimulus. It may still be
desirable that these applications wake up periodically and just let
the monitoring system know that it is still alive and ok (a
heartbeat).

**Container Scheduler and Virtual Machine Containers**

A virtual machine container is an isolated executing environment for a
virtual machine. 

The container scheduler ensures that each container gets a time slot
to execute.  Possibly, these containers could be implemented using a
"thread" feature of an underlying OS/RTOS/HAL or be given a certain
number of iterations of some main loop.

Containers should be isolated from eachother to allow different level
of criticality to different containers. High criticality containers
should be prioritised over low criticality containers.
 

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

	rectangle "Private Peripherals and Resources" as ppr

	rectangle "Peripheral Drivers\nRuntime Support" as pd

	
	vm -> mm 
	mm -> mem

	vm --> pd
	pd -> ppr

	caption A VM container consist of a virtual machine instance and a set of dedicated and private resources.

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

The virtual machine is based upon the Categorical Abstract Machine
(CAM) and consists of number of registers and and an execution
unit. (More details later)

**Execution Unit**

The execution unit reads instructions from code
memory at location stored in PC register and evaluates each
instruction over the state.

**Context Scheduler and Context List**

There is a list of contexts (separate instances of code and state)
that can execute in a time-shared fashion on the execution unit.

(TODO: Maybe cooperative scheduling at this level? This would require
"go to sleep" operations in the bytecode.)

The contexts that are executed on one VM are sharing the memory
resources of the Virtual Machine Container.



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

The context list consists of some number of "Execution Context to
Activate". A context is activated by copying the stored register state
into the VM. Putting a context to sleep works in the reversed way.

# Scheduling

## Virtual Machine Container Scheduling

Priority based scheduling with pre-emption between virtual machine
containers. The VMC scheduling is implemented on top of threading
functionality of the underlying HAL. If there are no threading
abstractions in the HAL, the scheduling can be implemented using a
periodic timer interrupt or explicitly allocating a number of
iterations of a main-loop to each VMC. 
   

## Cooperative scheduling within a Virtual Machine

A program may consist of a number of cooperating tasks that are never
pre-empted. The running task gives up the Execution Unit by executing
`sleep ns` instruction. 

# The Categorical Abstract Machine

# Instruction set

# Security Measures

(TODO: This needs to be thought out parhaps after doing some kind of a Threat Analysis) 

# Fault Tolerance and Reliability

(TODO: Try to build in a base set of functionality for fault tolerance and reliability)

# Communication

## Between contexts within a Virtual Machine Container

Contexts executing within a virtual machine container share a
heap. Data can easily be exchanged between tasks if both tasks know of
a common name referencing a heap structure.  Since the Heap is private
to the container and contexts never pre-empt eachother, no locking
mechanism should be needed for accesses to these shared structures.

## Between Virtual Machine Containers

Low criticality containers should not be able to influence the
execution of a higher criticality container in any way.  (TODO: What
mechanisms for communication do we need to add for Container ->
Container communication?)



## Thoughts 

Splitting concurrency up between internal to VM container via contexts
and external between containers open up to running VM containers on
different cores in parallel.

Management of other resources is also important. Communication
interfaces, sensors etc. I think it would be beneficial to assign such
resources to a VM Container and never allow the same interface to be
connected to more than one VM Container.
