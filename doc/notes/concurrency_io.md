## Concurrency and IO

### Design

Concurrency and I/O are intertwined problems. Concurrency arises when there is a possibility of non-linear control flow in the program. Blocking I/O operations is one of the principal reason for enforcing non-linear control flow(long running numerical calculations is the other). We can have a linear control flow if we choose to wait for all I/O operations but that leads to sub-optimal hardware usage.

A solution to avoid wasteful waiting cycles (for I/O) while avoiding non-linear control flow(like callbacks) is to enforce a synchronous model of concurrency which introduces *controlled non-determinism*. Modeling non-determinism is essential, given that a program will interact with inherently non-deterministic components like networks, interrupts etc. The synchronous model specifies what behaviour to attain when each path(in a non-deterministic choice) is taken. 

In addition to the synchronous model, `lightweight threads` allow us to prevent blocking(in a synchronous call) the main thread of operation and enables utilising the hardware(or saving its energy) while waiting on I/O operations. However once we introduce threads an important question arises on *how to handle communication among threads?* We hope to utilise a restricted form of message passing which uses `channels` to communicate among threads.

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
1. Channels. Channels can be thought of as a thread safe concurrent dequeue. The question is **who manages the memory or lifetime of a channel?** If channels are used for inter-thread as well as inter-container as well as thread-I/O driver communication it cannot reside in the heap of a single container or `vmc_t`. For inter-container communication it should inspect the stack(s) of both containers(stacks because we can have multiple threads in each container) and when references from all stacks are dead only then it should be deallocated.

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

Can we have a design where the channels are never deallocated? There will be a static number of channels and if any thread in any application requests more than the max number of channels we raise an exception which needs to be handled. Channels will be reused although as channels are typed we will have some runtime casting depending on the type of messages being sent and received.

3. **Should the VM scheduler be restricted to a container?** That is should each container have its own scheduler? This is at odds with the global channels design. Because the VM scheduler will now along with the HAL scheduler decide which thread in which container should receive/send message from which channel, maintain their mappings etc. A rephrase of this question could be :

*Should the HAL scheduler manage each container scheduler or should we have one global VM scheduler which is of the same rank as the HAL scheduler and they together make decisions regarding policies?*


### Implementation

#### Proposed API

```
channel : () -> Channel a
send : Channel a -> a -> Event ()
recv : Channel a -> Event a
choose : [Event a] -> Event a -- non-determinism
sync : Event a -> a
spawn : (() -> ()) -> ThreadId

guard, wrap, wrapabort other helper combinators
to build richer Events.
```

We add the following to the runtime:

#### Thread

This is inside each container.

```
Note : We use the term "thread" and "context" interchangeably
```

Currently represented as:

```C
typedef struct {
  cam_register_t env;
  UINT       pc;
  cam_stack_t    stack;
} Context_t;

```

We should add an identifier

```C
typedef uint16_t UUID; // limits to 65536 threads; should be configurable

typedef struct {
  UUID threadid;
  cam_register_t env;
  UINT       pc;
  cam_stack_t    stack;
} Context_t;

```

We need a `suspended thread queue`; We can do that using:

```C
typedef struct {
  ...
  Context_t  contexts[VMC_MAX_CONTEXTS];
  bool       context_used[VMC_MAX_CONTEXTS];
} vmc_t;

```

1 represents active; 0 represents ?? (suspended?! dead?! unused?!)

We need more than a `bool` to represent the thread states.

```C
typedef enum {
  UNUSED,
  ACTIVE,
  SUSPENDED,
  DEAD //GC to free the heap (eager GC to allow more thread creation)
       // Or do lazy GC. Only when `spawn` is called GC the DEAD threads stack pointers
       // and clean the stack, env, PC etc
} Context_state_t;

typedef struct {
  ...
  Context_t  contexts[VMC_MAX_CONTEXTS];
  Context_state_t  context_used[VMC_MAX_CONTEXTS];
} vmc_t;

```

*Question*
How do we know when a thread has terminated i.e its environment register can be wiped clean? Even if the stack is not used the environment register might be accessed by the parent thread.

*Answer*
When the main thread's stack holds no pointers to the child thread only then mark it as `DEAD`. In that case we do a lazy GC i.e only when `spawn` is called and we see there are threads which are marked `DEAD` we initiate their GC. 

*Tradeoff* 
When you have `DEAD` and `UNUSED` threads and you get a `spawn` call which thread should you use?
- Using the `DEAD` thread means do GC (slower) but keeps the heap cleaner (less garbage)
- Using the `UNUSED` thread prevents GC(faster) but keeps the heap dirty (more garbage)

#### What does `spawn` do?

Imagine the "entity" running the following is the `schduler`:

- Take the `contexts` array 
- Find the first `DEAD` or `UNUSED` context (GC might trigger here)
- Copy the stack, PC, env to that context
- Switch to child thread PC and evaluate it by passing ()
- Continue evaluation until you encounter something synchronous (like `send`)
- Mark that context as `SUSPENDED`
- Switch to the parent thread and continue the parent PC from after the `spawn** call

*Question*

How about the following program:
```OCaml
let f = let ch = channel () in
        let _ = spawn (\() -> let x = 389270**89270 in (*long running*)
                              sync (send ch x)) in
        (*maybe other operations*)
        sync (recv_ch)
```
When do you suspend the spawned thread above? According to the above we should suspend when we get to the synchronous `send` call. However the `389270**89270` will engage the ALU and block can we suspend then?

#### Channel

#### Event
