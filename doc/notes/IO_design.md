## IO Design

#### IO API


```
iochannel : IODriver -> IOChannel a -- typing?
send_io   : IOChannel a -> a -> Event ()
recv_io   : IOChannel a -> Event a
```


Button press and synchronous light blinking


```haskell
main = 
  let c     = channel () in
  let b_ioc = iochannel gpio1 in
  let l_ioc = iochannel led1  in
  let _     = spawn (button_process b_ioc c) in
  let _     = spawn (led_process    l_ioc c) in
  ()

button_process b_ioc c = forever $     -- C1
  let _ = sync $ recv_io b_ioc in
  sync $ send c ()_

led_process led_ioc c = forever $    -- C2
  let _ = recv c in
  let led_state = sync $ recv_io led_ioc in
  if led_state
  then sync $ send_io l_ioc False
  else sync $ send_io l_ioc True
```


```
RTS layer
---------
rdyQ -> 

b_ioc
  sendq     ->
  recvq     -> 
  io_driver -> button_gpio

l_ioc
  sendq     ->
  recvq     ->
  io_driver -> led_gpio
  
c
  sendq    ->
  recvq    -> 


Global_Queue -> 

Zephyr layer
------------
Container
  - mailbox -> 

Drivers
-------
UART
 - 1000 chars

```

#### Scheduler interaction with global queue

```

scheduler =
   if (empty container.rdyQ)
   then do
      handover to Zephyr
         { lower Zephyr layer
           zephyr receives interrupt
           queues on GlobalQueue
           call gq_logic
         }
     call scheduler
   else do
      let ctx = deque container.rdyQ
      execute ctx

type GlobalQueue = Queue (Driver, Message) - 5 bytes

-- globalQueue to RTS conversation
gq_logic =
 let (driver, msg) = dequeue globalQueue
     ioc = get_io_channel driver
     bool b = poll_recvq ioc.recvq
  in if b
     then do
       let (context, bool_ref) = deque ioc.recvq
           _ = mark bool_ref true
           _ = convert msg to cam_value
           _ = place `msg` in `context.env`
        in do -- set the receiver id to run
          context.pc++
          container.rdyQ.enqueue context
     else 
       ..nothing in recvq..
       ..drop message and handover to scheduler..
       .. this situation should not arise..

get_io_channel driver =
  ... loop through the iochannels in
      the container and look for the io_driver
      field associated with driver...
  ... need a way to distinguish drivers...
  ... UUID for drivers...

poll_recvq -- same as that in event.h

```
Scenarios of receiving interrupt

1. when readyQ is empty and an interrupt arrives
2. when we are in the middle of a bytecode execution

The lower level Zephyr can consistently keep the following
logic in both scenarios

```
  zephyr receives interrupt
  queues on GlobalQueue <- isr
  call gq_logic

```

In case 2 we might do

```
  zephyr receives interrupt
  queues on GlobalQueue <- isr
  finish bytecode execution
  call gq_logic
  proceed with the remanining bytecode
    execution until blocked

```


