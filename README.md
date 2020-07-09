# Sense-VM

SafE aNd SEcure Virtual Machine



## Thoughts

### Specification and testing
- 

### Source languages

- Functional programming languages

### Target platforms

- Resource constrained microcontroller based systems.
- Memory sizes of 128KB (or less ?) to a few megabytes.
- 32Bit target architectures? If possible make it compatible with 64Bit as well.

### Garbage collection and memory

- What will be the allocation (and garbage collection) unit of storage?
  - Target platforms such as the STM32F4 does not have a cache. All memory accesses are uniformly slow. (There is a cache for the flash memory.)

- Is the time it takes to do Garbage collection a problem? What GC algortithm?

- Heap consisting of cons-cells is easy to implement.

- Possible unit of allocation
  ```
  [Type info, Flags, GC-bits, safety and security bits | 32 bits data or ref | 32 bits data or ref]
  ```

### RTS language support features

- Message queues?
- Peripheral drivers?

### Scheduling

- Timing.
- Processes at different criticality levels. 

### Safety and Security features that are of interest

- What kind of fault tolerance mechanisms can we include?
  - Watchdog timers?
  - Error detection codes?
  - Redundancy?
    - N-Version programming and selection?
  - Snapshot and recovery?
  - Sandboxes - isolation?
    - Overwriting of memory - reading of memory I should not read.
- What security mechanisms can we include?
  - IFC?
  - Firmware authenticity check.
- Security mechanisms of target platform.
  - Arm TrustZone (only precent on more competent microcontrollers like M33).
  - Arm TrustZone Cryptocell 310 security system (NRF52840 has this feature).
  - Unique 96bit device IDs (STM32F4).
  - SDIO (Secure Digital IO) SD-Card interface?
  - SPI Communication with DMA and CRC.
  - Clock Security System.
  - IWDG: Watchdog peripherals (STM32F4 has 2 of these).
  - Secure boot features (NRF52840 has some features)
  - Secure erase (NRF52840)
  - Memory write protection.
  - Flexible memory controller (STM32F4).
  - Memory protection unit (NRF52).
  - Debug port disabling.
  - Tamper detection.
  - Power supply monitoring.
  - Temperature monitors (abnormal temperature profile may be a sign of tampering).
  - Secure firmware updates.
    - STM32 TF-M User Manual (UM2671).
  - Hardware crypto: AES, HASH, TRNG.
  - Readout protection.
  - What should be the response if tampering is suspected?
    - Safe default mode/value
    - ?
  
- System monitors, progress monitors, resource monitors



- Things to look at
  - Green hill software - Integrity
  - safe&secure RTOSes


  - Remote firmware updates - Danger
  - Parameterization. Change operating parameters remotely - Danger.



- Range of timing requirements
  - From very slow: smart home.
  - To very fast: connected cars?


- Communication
  - Ethernet - is comming on strong. 
  - FlexRay  - Thought to become a Safety bus 
  - CAN
  - LIN  - Very simple
  - UART
  - SPI
  - i2c
  - Bluetooth?
  - Wifi?
  - Cat-M1, NB-IoT?