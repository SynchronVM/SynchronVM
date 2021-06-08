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



## Researchy

1. Memory management - Look into alternatives to a stop the world GC
2. 