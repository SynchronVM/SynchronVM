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

Imagine the "entity" running the following is the `scheduler`:

- Take the `contexts` array 
- Find the first `DEAD` or `UNUSED` context (GC might trigger here)
- Copy the stack, PC, env to that context
- Switch to child thread PC and evaluate it by passing ()
- Continue evaluation until you encounter something synchronous (like `send`)
- Mark that context as `SUSPENDED`
- Switch to the parent thread and continue the parent PC from after the `spawn` call

*Question*

How about the following program:
```OCaml
let f = let ch = channel () in
        let _ = spawn (\() -> let x = 389270**89270 in (*long running*)
                              sync (send ch x)) in
        (*maybe other operations*)
        sync (recv ch)
```
When do you suspend the spawned thread above? According to the above we should suspend when we get to the synchronous `send` call. However the `389270**89270` will engage the ALU and block. Can we suspend then?

#### Channel

Channels are used for three types of communication:

- inter thread communication
- inter container communication
- thread-I/O driver communication

A tentative structure:

```C
typedef struct {
   UUID channel_id, //maybe not necessary
   pthread_mutex_t lock, // if sticking to C99 might have to roll out our own locks
                         // or maybe unnecessary in a single threaded runtime
   Queue sendq,
   Queue recvq
} Channel_t;
```

There should be an area outside the containers which will statically allocate channels:

```C
Channel_t channels[MAX_CHANNELS];
```

`MAX_CHANNELS` instead of a static number should be determined by looking at the structure of the program. Of course it is allocated before the program starts and is not dynamically created but the number can be detected from the structure of the program.

*Possible Optimisation*

```OCaml
(* Thread id 20 *)
let c = channel () in
let val = a large value in
sync (send val c)
```

If the receiver to the above channel `c` has not arrived then this program blocks. So we do not directly send the value to the channel. We simply register the thread id in the `sendq` of channel `c`.

```
Channel c
sendq -> 20 -> Nil //20 is the thread id
recvq -> Nil

```

Now as this blocks we sleep until a receiver arrives. After some time a receiver from a different thread id arrives and says

```OCaml
(* Thread id 30 *)
...sync (recv c) (* same c as above *)
```

we have

```
Channel c
sendq -> 20 -> Nil
recvq -> 30 -> Nil

```

Now on channel `c` threadid 20 is sending data and threadid 30 is going to receive. After doing the necessary synchronisation we can simply do the following as a *transaction*

- copy data from thread 20's stack to thread 30's stack (because thread 20 got suspended when it was waiting on send the top value of the stack will be the value, in this way we don't have to copy the value to the channel and so on)
- clear c.sendq head
- clear c.recvq head

*Optimisation 2*

Use meaningful (or semantic) UUIDs. If we see that thread 20 and thread 30 are from the same container id (the container id should be encoded in the thread's UUID) we can avoid a copy operation as well. Because they are from the same container, they share the heap so simply give thread 30 a pointer to the heap location of `val` in thread 20's stack.

The above approach would work fine for

- inter thread communication
- inter container(threads within containers) communication

#### How about `thread-I/O driver` communication?

The thread and I/O driver communication is a separate type of communication which somehow lies outside the model. Our general API is

```
send : Channel a -> a -> Event ()
recv : Channel a -> Event a
```

Now imagine we expect to receive data from an I/O driver if we simply do

```OCaml
... let ch = channel () in
    let a  = recv ch...
```

In the above it expects a value from the sender, but if the sender is an I/O driver how do we make it send data? We firstly need to specify the name of the driver, and also then the `send` is initiated asynchronously by the external world which is outside our program and programming model.

One proposed solution is having two special unidirectional channel creation functions for I/O. So our new API will have:

```
iochannelS : IODriver -> Channel a 
iochannelR : IODriver -> Channel a
```

The `IODriver` type is a representation of some kind of asynchronous I/O port which is handled by the runtime. Imagine we have

```
gpioDriver1 : IODriver
```

provided by the runtime where all the necessary interrupt handlers required by `GPIODriver1` is already supported in the runtime(interrupt handlers written in C which abstract over with a general API). So our original API changes very slightly:

```OCaml
... let ch = iochannelR gpioDriver1 in
    let a  = recv ch...
```

The only change was from `channel ()` to `iochannelR gpioDriver1`. Now the scheduler can easily support the general programming model and pre-empt the program when it reaches `recv ch`. Only when GPIO driver receives an interrupt, we handle that and awaken the remaining parts.

Runtime wise with two new functions we have two new channel types:

```C
typedef struct {
   UUID channel_id,
   pthread_mutex_t lock,
   Queue sendq,
   IODriver driver_details
} IOChannel_send_t;

typedef struct {
   UUID channel_id,
   pthread_mutex_t lock,
   Queue recvq,
   IODriver driver_details
} IOChannel_recv_t;
```

In the program when we reach `iochannelR gpioDriver1` we assume that the runtime already contains details related to interrupt handling for GPIO Driver1. All it does is, takes the thread id of program and enqueues that thread id to `recvq` of `IOChannel_recv_t` type.

Now, given we have a bridging system, we accept the hardware interrupts and transform them into software messages (stored in a buffer). Once the scheduler determines its time to wake up the correct thread it takes that software message and straight away pushes it to the stack of the suspended thread.

For `send` it will do something similar. If the bridge buffer is full it will suspend the send threads. Once the buffer has some space it will copy the top of the stack of the suspended send threads to the buffer of the concerned I/O driver. The "concerned I/O driver** is stored in the `driver_details** field.


*Alternate IOChannel design*

API :

```
iochannel : IODriver -> Channel a
```

Runtime :

```C
typedef struct {
   UUID channel_id,
   pthread_mutex_t lock,
   Queue sendq, // when receiving messages this remains empty
   Queue recvq, // when sending message this remains empty
   IODriver driver_details
} IOChannel_t;
```



#### Event

An `Event` is the intermediate abstraction that is used to **compose** and build larger communication blocks. `Event`s distinguish Concurrent ML from all other CSP implementations and APIs and (in my opinion) makes all communication truly compositional.

Eg:

```OCaml
(* foo : Event () *)
let foo =
    let c1 = channel () in
    let c2 = channel () in
    let c5 = channel () in
    let c6 = channel () in
    choose
      [ choose [ send c1 a
               , wrap (recv c1) (\m1 -> sync (send c2 a))
               ]
      , guard (\() -> pre-sync action;
                      send c2 a))
      , wrap (choose [ recv c5
                     , recv c6
                     , wrap (send c1 a) (\() -> sync (recv c2))
                     ]
             ) (\m3 -> sync (send c5 m3))
      ]
```

The above is just an example, but represents how we can *compositionally* build a complex Event out of *base event constructors* (In CML literature `send` and `recv` are called "base event constructors"), choice and other pre- and post-synchronisation helpers. The function foo represents an Event but we can see that Event is a composition of a complex tree of several events. So when we say

```OCaml
sync foo
```

It takes the entire Event tree, does multiple races (using `choose`) and when one of the action wins cancels all of the others. In the runtime an Event is represented as a tree, which will be allocated on the VM heap. 

Runtime representation of Event:

The structure of an `Event` is directly related to the main combinators that operate on it. The most important combinator is

```OCaml
sync : Event a -> a
```

The structure of an `Event` in CML is dictated by the operations that the sync operation would do. In SML it is represented as:

```SML
datatype 'a base_evt = BEVT of {
    pollFn  : unit -> bool,
    doFn    : unit -> 'a,
    blockFn : (bool ref * 'a cont) -> unit
  }
and 'a event = EVT of 'a base_evt list

(* 'a cont represents a suspended continuation *)
```

We need to distinguish between 3 different operations:

```
send c 5
sync (send c 5)
spawn (() -> send c 5) -- or spawn (() -> sync (send c 5))
```

```
send c 5
```

The above creates a record of functions. None of the functions are applied and they will be used when `sync` is called. An Event says **what need to be done**. Doesn't actually do it. The doing part happens after `sync` is called. It is a recipe.

```
sync (send c 5)
```

It takes the record of functions from above and **actually does** the operations.

First it polls to see if any operation is ready to be synchronised. It does this by going through that `Event`'s channels and seeing if the corresponding queue has any synchronizer.

If none of the operations are ready to be synchronized it calls the `blockFn` of the respective base events which internally takes the `copy of stack` and stores it in the suspended queue and registers the thread id to the respective channel's queue. It then switches to work with the other elements of the `readyQ`.

If any one operation is ready to be synchronised it executes the `doFn` of the `Event` which mainly `moves the current stack` into the `readyQ`. Also it ensures the desired message copying happens. This is the main role.

The `pollFn` should take care of dequeuing from the `readyQ` failed actions.

```
spawn (() -> send c 5) -- or spawn (() -> sync (send c 5))
```

`copy the stack` to `readyQ`.

Question: Execute which bytecode after copying next?
Choices: Either the parent or the bytecode of the spawned thunk (child). Might need a way to distinguish the end of a child thread bytecode.



### References

- [John Reppy's continuation based Parallel Concurrent ML implementation](https://cliplab.org/Conferences/DAMP08/papers/7.pdf)
- [Avik Chaudhuri's embedding of Concurrent ML in Concurrent Haskell](https://www.cs.umd.edu/~avik/projects/cmllch/)
- [Rob Pike's CSP based Newsqueak runtime implementation](https://swtch.com/~rsc/thread/newsquimpl.pdf)
