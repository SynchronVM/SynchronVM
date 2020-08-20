


# Sense-VM architecture 







## Overview


## Single Threaded VM 


## Concurrent VM 

```plantuml

package "Virtual Machine" { 

	frame "Registers" { 
		[PC] 
	    [SP] 
		[CP]
		[EP]
		
	}
	
	[Context List]
	[Scheduler]
	[Execution Engine]
	[Heap]
}


node "Avtive Execution Context" { 
	[Code Memory] 
	[Stack]
	[PC-RESUME]
	[Environment]
	[Execution Parameters]
}

[Scheduler] --> [Execution Parameters]

```

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
	[Scheduler] --> [Execution Parameters] : Load
	

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
 


