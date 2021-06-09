# Future Work

## unclassified

1. Preemptive scheduling of important interrupts.
2. Parallelism between containers on multicore. ESP32 example platform
3. Serialising and deserialising compound data when send/recv from driver or over network (or between containers).
4. Serialising and deserialising environments.


## Engineering

1. Multiple containers.
2. Priorities on containers and tasks.
3. Time. Preferably some way of handling time that fits into the Channel abstraction.
4. Container to container channels.
   - Try to also make these fit in the same channel abstraction.
   - These channels will go via the RTS and probably internally work
     very similarly to how a channel connected to a driver works.
   - Poll-channel
     - can be implemented as sync (choose (recv c) (timeout x))
     - what exactly is a timeout? Timer API needed
     ``` 
       	  timeout : Time -> ()
          timeout t = let c = genTimeout t  --- 
                      in recv c 
     ```
     - `genTimeout` can be implemented as `RTSCALL x`
     - Channels are a limited resource!
     - Timeout channels will have a known lifetime (at runtime) - Can that be exploited?

    i. One interesting idea - Have a hidden time process and the process gets blocked on that process which sends a message after time t. 
    ii. Timed channels - useful for implementing timeouts. Can we implement this simply using what we already have?

5. Should the time system operate on absolute time or relative?
   - The SSM project is making good arguments for absolute time.

6. The environment. We represent the environment as a nested tuple. The advantage is that the tuple like structure encodes lookup + scope well. However lookup a variable can end up traversing the heap a lot. If we introduce a symbol table we need to think a bit about how we maintain the scoping, but no traversal required, lookup is O(1). Tradeoffs.


## Researchy

1. Memory management - Look into alternatives to a stop-the-world GC
   - Under the assumption that IoT applications do nothing most of the time,
     expensive bookkeeping operations should be pushed until just before
     going to sleep if possible. 

2. The security track
   - Information flow policys?
   - Security on the edges? what edge protocols?
     - Encryption?    
     - Edge technologies: I2C, SPI, CAN, BLE, WIFI ????
       - How bad are they from safety perspective
   - Security and container isolation.


3. The safety track
  - Front-end languages or sublanguages for higher criticality.
    - Maybe limit event structures (reduced functionality choose?).
    - Limit recursion.
    - Limit closure creation.
    - Limit spawn.
  - Possible that additional Op-codes are necessary (no problem we have space).

  - Sefety and container isolation.

4. Study/experiment with efficient list representation in the heap (https://ieeexplore.ieee.org/document/1663507)


## Researchy

1. Memory management - Look into alternatives to a stop the world GC
2. Implement FRP as a library in CamIoT. Why? A: Utilising the send, recv primitives might help implementing push-based FRP.
3. How does the channel-based model extend to containers and distribution?

