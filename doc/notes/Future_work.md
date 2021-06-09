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
5. Should the time system operate on absolute time or relative?
   - The SSM project is making good arguments for absolute time. 

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