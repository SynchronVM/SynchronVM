### TIME

Introducing time to SenseVM using `syncT` or `timed synchronisation`

```
syncT : Time -> Time -> Event a -> a
         ^       ^
         |       |
     relative    |
     baseline    |
               deadline
               relative to 
               relative baseline
```

Eg: Playing a note and handling volume increase/decrease

```Haskell
inp_c   = channel () -- I/O channel for receiving volume input
music_c = channel ()
dac_c   = channel () -- I/O channel for writind to DAC

volume_p v1 =
  let v2 = sync $ choose [ recv inp_c
                         , wrap (send music_c v1) (\_ -> v1)
                         ] 
   in volume_p v2

player_p v1 =
   let v2 = syncAfter 300 (wrap (recv music_c) recv_handler)
    in player_p v2
   where
      recv_handler v = let _ = sync $ send dac_c v
                        in v

syncAfter : Time -> Event a -> a
syncAfter t = syncT t 0
```
