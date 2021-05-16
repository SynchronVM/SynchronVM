#### PROCESS TERMINATION

```
chan : Channel Int
chan = channel ()

process1 : () -> ()
process1 void = sync (send chan 5)

main =
  let _ = spawn process1 in;
  let v = sync (recv chan) in
  v
```

In the above program the context switch proceeds something like this:


```
C0 gets blocked at sync(recv chan)
C1 gets scheduled
C1 arrives at sync(send chan)
C1 puts itself at rdyQ and C0 is scheduled
C0 terminates without issues

```
Bytecode for the above
```
CLEAR;
CALLRTS 1; -- channel()
PUSH;
CUR 1;
CONS;
PUSH;
SND;
CALLRTS 0; -- spawn
CONS;
REST 2;
CALLRTS 3; -- recv
CALLRTS 4; -- sync
STOP;
lab_1 : PUSH;
SND;
CONS;
REST 2;
MOVE;
QUOTE (LInt 5);
CALLRTS 2; -- send
CALLRTS 4; -- sync
RETURN

```

The problem is the RETURN at the end. In this program C0 gets scheduled and RETURN is never hit. But it could be problematic. So the solution is a label graveyard. 

#### Label Graveyard
A label graveyard is a point where all spawned processes land when they hit RETURN. So as result **when spawning ensure that the stack of that spawned process is pushed with the address of the label graveyard**. The label graveyard is:

```
lab_n : STOP
```

What happens at STOP?

STOP currently does nothing it should call `dispatch` and set the current context_used flag to FALSE. What happens in the above program then?

New bytecode:

```
CLEAR;
CALLRTS 1; -- channel()
PUSH;
CUR 1;
CONS;
PUSH;
SND;
CALLRTS 0; -- spawn
CONS;
REST 2;
CALLRTS 3; -- recv
CALLRTS 4; -- sync
STOP;
lab_1 : PUSH;
SND;
CONS;
REST 2;
MOVE;
QUOTE (LInt 5);
CALLRTS 2; -- send
CALLRTS 4; -- sync
RETURN
lab_graveyard: STOP

```

When STOP is encountered the first time the rdyQ contains C1.

We call `dispatch`:

C1 is back and then it does PC++ and comes to RETURN; The RETURN then jumps to 
the `lab_graveyard` which calls STOP. We again do `dispatch`. No more processes in the `rdyQ`. So `current_running_context = UUID_NONE`. The program stops.


Concrete steps:

```
1. Label Graveyard : LABEL - STOP - codegeneration
2. When I spawn - push on the stack the label graveyard
3. When STOP is encountered set context.used = false and call dispatch()
4. Scheduler should check if all contexts are false to determine if the program is stopped
```
