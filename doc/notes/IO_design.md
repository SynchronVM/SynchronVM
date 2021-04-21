## IO Design

#### IO API


```
iochannel : IODriver -> IOChannel a -- typing?
send_io   : IOChannel a -> a -> Event ()
recv_io   : IOChannel a -> Event a
```


Button press and synchronous light blinking


```ocaml
main = 
  let c     = channel () in
  let b_ioc = iochannel gpio1 in
  let l_ioc = iochannel led1  in
  let _     = spawn (button_press b_ioc c) in
  let _     = spawn (button_press l_ioc c) in
  ()

button_press b_ioc c = forever $
  let _ = sync $ recv_io b_ioc in
  sync $ send c ()_

led_process led_ioc c = forever $ 
  let _ = recv c in
  let led_state = sync $ recv_io led_ioc in
  if led_state
  then sync $ send_io l_ioc False
  else sync $ send_io l_ioc True
```
