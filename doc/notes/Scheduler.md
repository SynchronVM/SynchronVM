## Scheduler

There will be some interesting challenges in the scheduler for real-time scenarios. Consider,

```Haskell
foo = ... syncT 0 3 $ recv c1 ...

bar = ... sync $ send c2 5 ...

baz = ... sync $ recv c2 ...

quux = ... sync $ send c1 10 ...

main = ....
  spawn foo;
  spawn bar;
  spawn baz;
  spawn quux;
  ....
```

In the cooperative scheduler we schedule threads in the order that we encounter them in the program file. So we will simply follow the order that `main`
has spawned them in.

Step 1. We schedule `foo`. Arrive at `syncT` and realise the baseline is 0 and attempt to synchronise but get blocked. Dispatch next thread.

Step 2. We schedule `bar`, might take an arbitrary amount of time till we get block on the `sync` call and dispatch the next thread.

Step 3. We schedule `baz`, take an arbitrary amount of time to do the concerned tasks and unblock `bar`. Finishing this we dispatch the next thread.

Step 4. We finally arrive at `quux` which can unblock `foo`. But by the time we are ready to unblock `foo` the deadline of 3 ticks might have passed.

What went wrong?

We schedule in the order `foo -> bar -> baz -> quux` whereas the ideal order should have been `foo -> quux -> bar -> baz`.

So when scheduling real-time tasks, we not only have to schedule the real time task first but also an untimed task which might unblock this timed task.

How?
