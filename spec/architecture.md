
# Sense-VM architecture 


## Overview


## Single Threaded VM 


## Concurrent VM 

```plantuml
left to right direction

node "Virtual Machine" { 
	frame "Registers" { 
		[PC] 
	    [SP] 
		[CP]
		[EP]
		
	}
	
	[Context List]
	[Execution Engine]
	[Heap]
	
	Frame "Manager" {
	[Scheduler]
	[Progress Monitor]
	}
}


node "Avtive Execution Context" { 
	[Code Memory] 
	[Stack]
	[PC-RESUME]
	[Environment]
	[Execution Parameters]
}




node "Runtime Support" { 
	frame "Hardware Abstraction Layer" {
		[Drivers]
		[Interrupt routines]
		[Communication]
	}

	frame "Virtual Machine Container" {
		
	}
	
	frame "Power Management" { 
		[Power Monitor] 
		[Sleep Manager] 
	
	}
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
 


