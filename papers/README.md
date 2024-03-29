### RELATED WORK

- [Pypy - Generating VM for dynamic languages](https://sites.cs.ucsb.edu/~ckrintz/classes/s20/cs263/readings/pypy-vm-construction.pdf)
- [Pycket - A JIT compiled VM for Racket written using Pypy](http://homes.sice.indiana.edu/samth/pycket-draft.pdf)
- [ZINC - The ML abstract machine by Xavier Leroy](https://hal.inria.fr/inria-00070049/file/RT-0117.pdf)
- [Categorical Abstract Machine](https://www.researchgate.net/publication/228940729_The_Categorical_Abstract_Machine_Basics_and_Enhancements)

#### Memory Management(Static)

- [A Brief Introduction to Regions](https://dl.acm.org/doi/abs/10.1145/301589.286882)
- [Memory Efficient Hard Real-Time Garbage Collection](http://liu.diva-portal.org/smash/get/diva2:20899/FULLTEXT01.pdf)
- [ASAP: As Static As Possible memory management](https://www.cl.cam.ac.uk/techreports/UCAM-CL-TR-908.pdf)
- [Towards region-based memory management for Go](https://dl.acm.org/doi/abs/10.1145/2247684.2247695)
- Static Analysis
  - [Higher Order Escape Analysis](https://link.springer.com/content/pdf/10.1007/3-540-52592-0_61.pdf)
  - [Escape Analysis on Lists](https://cs.nyu.edu/~goldberg/pubs/pg92.pdf)
  - [Escape analysis: correctness proof, implementation and experimental results](https://dl.acm.org/doi/abs/10.1145/268946.268949)


#### Real-Time GC
- [List processing in real time on a serial computer](https://dl.acm.org/doi/abs/10.1145/359460.359470)
- [A real-time garbage collector based on the lifetime of objects](https://dl.acm.org/doi/abs/10.1145/358141.358147)
- [RTMLTon](https://link.springer.com/chapter/10.1007%2F978-3-030-39197-3_8)
- [One Pass Real-Time Generational Mark-Sweep Garbage Collection](https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.42.7791&rep=rep1&type=pdf)
- [A Hard Look at Hard Real-Time Garbage Collection](https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.69.3943&rep=rep1&type=pdf)
- [Semi-Automatic Region-Based Memory Management for Real-Time Java Embedded Systems](https://ieeexplore.ieee.org/document/4296838)
  - This identifies a very common problem in region-based memory managers - all objects gets allocated to one long-lived region and that particular region
    never ends up being destroyed, leading to memory leaks. The paper believes this problem is common enough to be given its own name - *region explosion syndrome*.
    It mentions several works including, ML-based region-inferencing algorithms as well as programmer-managed manual regions suffering from the region
    explosion syndrome.
    
    Key idea of this paper is - "The generally admitted generational hypothesis states that connected objects will tend to have a similar lifetime. Accordingly, we propose to put each data structure (i.e. each maximal set of connected objects) in a distinct region. The idea is that most objects are either short-lived, and so they should be placed in a short-lived region, or long-lived, because they are integrated in a large lasting structure, and they should be placed together with the rest of the structure." The paper then presents its main contribution - *a static analysis pass to over-approximate the object connectivities* and accordingly place connected objects in the same region. The work involves the programmer by warning of potential leaks and the programmer can accordingly restructure their program.
 - IBM's Metronome Collector
   - [The canonical paper](https://dl.acm.org/doi/abs/10.1145/640128.604155)
   - [Popular presentation](https://dl.acm.org/doi/10.1145/1217256.1217268) - The broad idea seems to be about making the time of the application pause deterministic rather than removing pauses altogether. In the paper when the pause times are plotted they are almost always equally spaced. The collector requires two important application parameters - the maximum live memory that it uses and the maximum long-term allocation rate.
 - [Schism: Fragmentation-Tolerant Real-Time Garbage Collection](https://dl.acm.org/doi/abs/10.1145/1809028.1806615)


#### Stack vs Heap for closures
- [An Empirical and Analytic Study of Stack vs. Heap Costfor Languages with Closures](https://www.cs.princeton.edu/~appel/papers/stack2.pdf)

#### Real Time and Functional Languages
- [A survey of real‐time capabilities in functional languages and compilers](https://onlinelibrary.wiley.com/doi/full/10.1002/cpe.4902)
- [Actor Systems for Realtime computations - Henry Baker's 1978 PhD Thesis](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.18.4791&rep=rep1&type=pdf)


### RELATED WORK 2

- [The Space Cost of Lazy Reference Counting](https://dl.acm.org/doi/10.1145/982962.964019)
  - Section 3 summarizes lazy deletion of garbage. While the problems associated with lazy deletion might not immediately occur
    in our heap, but when we introduce arrays and other forms of contiguous allocations we will encounter the issues listed in the paper.
    The paper also includes a good benchmark on the pause times of reference counting (while freeing). Note it remains to be seen how these
    benchmarks translate to a functional language.
- [Dynamic storage allocation : A survey and critical review](https://link.springer.com/chapter/10.1007/3-540-60368-9_19)
- [An efficient, incremental, automatic garbage collector](https://dl.acm.org/doi/abs/10.1145/360336.360345)
  - Uses two tables MRT(Mutireference table) and ZCT(Zero count table) two segregate references based on their count. There is an additonal
    variable reference table (VRT) "which contains in a hash table all those pointers referenced from the stack. Then the ZCT can be scanned, 
    and any cell not referenced from the VRT can be reclaimed.". It doesn't account for cycles and uses GC to collect them.
- [Ulterior Reference Counting: Fast Garbage collection without a long wait](https://users.cecs.anu.edu.au/~steveb/pubs/papers/urc-oopsla-2003.pdf)
- [Counting Immutable Bean. RC for purely functional languages](https://arxiv.org/pdf/1908.05647.pdf)
- [Practical Principle of Least Privilege for Secure Embedded Systems](http://sjero.net/pubs/2021_RTAS_Patina.pdf)
