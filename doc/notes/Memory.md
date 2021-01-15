## Memory

Following are some notes on memory layout and management.

#### Long lived memory

Events are dynamic structures. In a program like the following:

```
choose [e1, e2, e3]
```

We allocate a list on our heap. However frequently we do the following:

```
forever $....
    choose [e1, e2, e3]
```

In practise the Event list is a long lived data structure which occupies the heap which should be kept free for more short lived allocations. This is an opportunity to use the `array-memory`. For long lived (or permanent) events we can move them to the array-memory which is outside the bounds of the garbage collector, keeping the heap free from fragmentation.

This is remniscent of a generational collector which keeps short lived objects in a Nursery and long lived object in a more permanent memory space.

#### Fairness

For the following program

```
choose [e1, e2, e3]
```

if we simply take a list and move it at in a left-to-right fashion we may create a left-biased concurrency model. The right most elements will continue starving especially for long lived events.

A strategy for fairness utilises a rotating queue like structure. Imagine if all 3 events are always ready to synchronize we do the following in the memory:

```
e1 -> e2 -> e3

pick e1 and return it the first time and rearrange

e2 -> e3 -> e1
```

Similary, next time when we come across the choice we do

```
e2 -> e3 -> e1

pick e2 and return it the first time and rearrange

e3 -> e1 -> e2
```

and so on. If an element in the middle gets selected we have to do something similar and put that element in the back of the queue. However to do that in the middle we would need a reference to the previous element when we are executing such a scheme.

Question: Does this sort of scheme fit well for the very static array memory?
