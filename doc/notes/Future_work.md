# Future Work

## unclassified

1. Preemptive scheduling of important interrupts.
2. Parallelism between containers on multicore.
3. Serialising and deserialising compound data when send/recv from driver or over network (or between containers).
4. Serialising and deserialising environments.


## Engineering

1. Multiple containers.
2. Priorities on containers and tasks.
3. Time. Preferably some way of handling time that fits into the Channel abstraction.
    i. One interesting idea - Have a hidden time process and the process gets blocked on that process which sends a message after time t. 
    ii. Timed channels - useful for implementing timeouts. Can we implement this simply using what we already have?
4. Study/experiment with efficient list representation in the heap (https://ieeexplore.ieee.org/document/1663507)



## Researchy

1. Memory management - Look into alternatives to a stop the world GC
2. Implement FRP as a library in CamIoT. Why? A: Utilising the send, recv primitives might help implementing push-based FRP.
3. How does the channel-based model extend to containers and distribution?
