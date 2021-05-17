#### DRIVER IDENTITY


There are 2 layers of identification of drivers required

1. At the backend when initialising the drivers in VMC.h
2. At the frontend language when a programmer expresses an intent in one of the drivers.

###### Backend

`VMC.h` currently has

```C
  ll_driver_t lld;
  
  #if VMC_CONTAINER_1_USE_BUTTON_0
  #include <ll_button.h>
  if (ll_button_init(&lld, drv_num, vm_containers[VMC_CONTAINER_1].backend_custom, 0)) {
    vm_containers[VMC_CONTAINER_1].drivers[drv_num] = lld;
    drv_num++;
  }
  #endif
```


A message arrives in the following struct

```C
typedef struct ll_driver_msg_s{
  uint32_t driver_id;     // Index into an array of drivers maintained by "low-level" 
  uint32_t data;          // Data payload, driver specific message or pointer  
  uint64_t timestamp;     
} ll_driver_msg_t;

```

The `drv_num` in the first snippet and `driver_id` in the struct are the same identifiers.
The scheduler uses `poll_msg(vmc_t *vmc, ll_driver_msg_t *msg);` to get a message and then
looks at the `driver_id` to know that `vmc.drivers[driver_id]` is the one who got the message

So the driver needs to know this driver_id number before sending a message.

###### Frontend

If I do

```
let c = channel () : Bool in 
let _ = _spawnExternal c ??
```

What goes in the ??

Surely it should be the same driver_id or `drv_num` discussed in the Backend section. However it being a number or a
plain string spoils the type safety of the program.

To tie these loose ends we can have a yaml config file:

```yaml
# the button0, button1 etc on the left are
# the Zephyr names used in the overlay files
- buttons: Button #use CAPS for the SenseVM type generation
    button0: 0
    button1: 1
    button2: 2
- leds: LED
    led0: 3
```

We can have `sensevm-dt` tool which will parse this file and emit the following:

```Haskell
-- Filename: IODrivers.cam

data IODriver = Button Int | LED Int

button0 = Button 0
button1 = Button 1
button2 = Button 2

led0 = LED 3
```

Now these indices 0,1,2,3 refer to the indices of the `drivers` array in VMC.h

Is it possible to automate the generation of a C file which does the following:

```C
  ll_driver_t lld;

  // we have the button index say 0
  #include <ll_button.h>
  if (ll_button_init(&lld, 0, vm_containers[VMC_CONTAINER_1].backend_custom, 0))   {
    vm_containers[VMC_CONTAINER_1].drivers[0] = lld;
  }

```
