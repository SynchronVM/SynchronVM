


# Send and recv compound data : Thoughts about future work


- Sending and recv compound data between two software threads (that
  share a heap) is not problematic. These will just exchange a heap
  pointer and be done.

- The problem arises when you send or recv compound data between two
  containers or between a container and a driver.
  Here the problem is that two separate containers do not share a heap, so
  the heap structure that A wants to send to B has to be recreated on B's heap.
  To do this some kind of serialization and deserialization is needed.

  The CAM machine is for statically typed languages and our "CamIoT"
  language is statically typed. This means that type information is
  not explicitly available at runtime. Rather the concepts of "types"
  have all been integrated in an implicit fashion into the instruction
  stream.  An example of this is that there is no way us to know that
  a particular heap reference is a list or a tuple, but the
  instruction sequence that operates on this heap reference is
  designed specifically for the list or the tuple case depending on the
  type information that was available at compile time.

  
- Thoughts about container - driver compound data transfer.  When it
  comes to a driver, we cannot (or want not) to send a reference to
  the heap and let the driver traverse the heap structure. This would
  be complicated and fragile and would need to interface the lowest level
  aspects of our CAM VM with the garbage collector. No!

  One way to resolve this is to compile serializers/deserializers into
  the bytecode stream when encountering a send or recv of compound
  data between containers or between container and driver.

  Send from container to driver:
  '''
  sync (send channel ref-to-compound-structure)
  ''' 

  If we at compile time know that the "ref-to-compound-structure" is a list
  of bytes one possible thing to do is the following:

  generate bytecode that does this:
  1. request memory from a pool.
  2. generate opcodes that loop through the list in heap memory
     and write each byte to the requested memory.
  3. sync in the normal way, letting the driver know that there
     is a chunk of memory to process.

  Things that can go wrong:
  1. the pool of memory has no free chunks.


  Recv from driver to container:
  '''
  let a = sync (recv channel)
  ''' 

  This is the case where 'a' is compound data and a heap structure should
  be created.

  When this sync "succeeds", the RTS will get a pointer to a memory chunk
  from the pool. 

  generate bytecode that does this, based on the type of a:
  1. generate an opcode sequence that traverses the memory chunk
     and recreates the heap structure.

  Things that can go wrong:

  1. We can run out of heap space while traversing the memory chunk and
     need to do a garbage collection pass.


  Positive side-effects:
  1. If we can transfer large data by usage of chunks from a bool, then
     we can fix the size of ll_read and ll_write. In that case maybe these two
     functions should read and write a single 32bit value (uint32_t).
     
  
  
  
  
