### Bughunt

Tracking some critical bugs and their possible causes here:

#### Bug 1 - Stack Overflow of CAM Stack

This program will cause a stack overflow of the CAM stack:

```Haskell
noteC : Channel Int
noteC = channel ()


playerP : () -> ()
playerP void =
  let _ = sync (send noteC 10) in
  playerP void

mutrec
  recvHandler : Int -> ()
  recvHandler i = tuneP ()

  tuneP : () -> ()
  tuneP void =
    let _ = sync (wrap (recv noteC) recvHandler) in
    tuneP void


main =
  let _ = spawn playerP in
  let _ = spawn tuneP in
  ()

```

Reason for stack overflow - Inside tuneP when wrap is executed, we place the jump address on the stack so that we remember after returning from wrap where we should jump back to. We want to return to the `let _ = ..` part and continue execution. This works when the wrap is not mutually recursive. But in this program the recvHandler itself is a recursive call to the function and it never returns. So we keep going through the body of tuneP and accumulate a number of jump addresses on the stack till the stack overflows.

Possible Fix: It is a hard fix but some how the bytecode sequence has to be studied and seen if the wrapped function is itself tail recursive. If it is, then we have to introduce a special kind of wrap which doesn't store the jump address on the stack. But finding out if the label being wrapped is tail recursive from the bytecode sequence is very hard. 
