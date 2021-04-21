## IO Design

#### IO API


```
iochannel : IODriver -> IOChannel a -- typing?
send_io   : IOChannel a -> a -> Event ()
recv_io   : IOChannel a -> Event a
```




```ocaml
foo b_ioc l_ioc = forever $
  let c = sync $ recv_io b_ioc in 
  let led_state = sync $ recv_io l_ioc in 
  if (led_state)
  then sync $ send_io l_ioc False
  else sync $ send_io l_ioc True

```
