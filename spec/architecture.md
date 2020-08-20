
# Sense-VM architecture 


## Single Threaded VM 



## Concurrent VM 


```plantuml
node "Virtual Machine" as vm { 
	frame "Registers" { 
		[PC] 
	    [SP] 
		[CP]
		[EP]
		
	}
	
	[Context List]
	[Execution Engine]
	
	Frame "Manager" {
	[Scheduler]
	[Progress Monitor]
	}
} 
```

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

```plantuml
node "Virtual Machine Container" {
	
   	rectangle "Memory" as mem {
		rectangle "Heap Memory" 
		rectangle "Stack Memory"
		rectangle "Constants Memory"
		rectangle "Array Storage Memory"
		rectangle "Code Memory"
	}
	rectangle "Virtual Machine" as vm
		
	rectangle "Memory Management\nRuntime Support" as mm
	
	vm -> mm 
	mm --> mem
	
}
``` 

<!-- [Scheduler] -> [Execution Parameters] -->
<!-- [Scheduler] -> [Sleep Manager]  -->

### Activation of a context 

```plantuml

	frame "Registers" { 
		[PC] 
	    [SP] 
		[CP]
		[EP]
		
	}
	
	[Scheduler]

	node "Avtive Execution Context" { 
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
 


