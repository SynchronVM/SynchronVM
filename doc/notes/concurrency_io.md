## Concurrency and IO

### Design

Concurrency and I/O are intertwined problems. Concurrency arises when there is a possibility of non-linear control flow in the program. Blocking I/O operations is one of the principal reasons for enforcing non-linear control flow(long running numerical calculations is the other). We can have a linear control flow if we choose to wait for all I/O operations but that leads to sub-optimal hardware usage.

A solution to avoid wasteful waiting cycles (for I/O) while avoiding non-linear control flow(like callbacks) is to enforce a synchronous model of concurrency which introduces "controlled non-determinism". Non-determinism is essential given that a program will interact with inherently non-deterministic components like networks, interrupts etc. The synchronous model specifies what behaviour to attain when each path(in a non-deterministic choice) is taken. 

In addition to the synchronous model, `lightweight threads` allow us to prevent blocking(in a synchronous call) the main thread of operation and enables utilising the hardware(or saving its energy) while waiting on I/O operations. However once we introduce threads an important question arises on how to handle communication among threads? We hope to utilise a restricted form of message passing which uses `channels` to communicate among threads.

`Channels` are dynamically created, synchronous, rendezvous mechanisms which will be used for
- inter thread communication
- thread-I/O driver communication
- inter container communication (containers are not currently part of the programming model but an application level abstraction)

The final and very important component of our design is a `scheduler`. The schduler does the following
- prevents starvations of threads
- maintains a "thread id to I/O driver" mapping for threads which have requested communication (send or receive)
- suspends blocking threads and sets the program counter on the active threads
- informs suspended threads about available I/O data
- responsible for eager handling of interrupts and clearing buffers which would allow more interrupts to arrive
- prioritise thread(as well as container) wakeups

We will have a HAL(hardware abstraction layer) supplied scheduler which will be responsible for scheduling containers. The VM scheduler should communicate with the HAL scheduler and together decide which thread and as a result which container it should prioritise waking up when a series of interrupts arrive. 

```
Concurrency & I/O = Lightweight threads + channels + scheduler
```

#### Design Questions
1. Channels. Channels can be thought of as a thread safe concurrent dequeue. The questions is **who manages the memory or lifetime of a channel**. If channels are used for inter-thread as well as inter-container as well as thread-I/O driver communication it cannot reside in the heap of a single container or `vmc_t`. For inter-container communication it should inspect the stack(s) of both containers(stacks because we can have multiple threads in each container) and when references from both stacks are dead only then it should be deallocated.

2. Related to point 1. A proposed design where the entire application will have a separate memory area dedicated to channels like:

```C
#define MAX_CHANNELS 100
typedef struct {
 ...
} vmc_t;

typedef struct {
 ....
 Channel_t channels[MAX_CHANNELS]
} Global_Vars_t;

```

Can we have a design where the channels are never deallocated? There will be a static number of channels and if any thread in any application requests more than that number of channels we raise an exception which needs to be handled. Channels will be reused although as channels are typed we will have some runtime casting depending on the type of messages being sent and received.

3. **Should the VM scheduler be restricted to a container?** That is should each container have its own scheduler? This is at odds with the global channels design. Because the VM scheduler will now along with the HAL scheduler decide which thread in which container should receive/send message from which channel, maintain their mappings etc. A rephrase of this question could be "Should the HAL scheduler manage each container scheduler or should we have one global VM scheduler which is of the same rank as the HAL scheduler and they together make decisions regarding policies?**

### Implementation

*Coming soon*
