Forth Interpreter
-----------------

When I was first learning about forth, through books
like _Starting Forth_, I threw together this interpreter
in java.

This is from the Java 7 timeframe, so I didn't have method
pointers (class::method) or lambdas. Those would have both
helped with the way I look up and call opcodes.  Otherwise,
for a quick-and-dirty bare-bones forth, I think this 
did the job.

You have the primitive forth words segregated into separate
java files by category, and a lot of boot-strapping happens
in a forth source-file that's bundled into the JAR file (BASE.FTH).

I learned a lot about how you can generate the language as you
go... as in, defining the syntax for comments and loops in BASE.FTH rather
than in primitive java. It was a mind-expanding experience.

The first implementation was actually closer to a proto-factor
(see [factorcode.org](http://factorcode.org)), in that each
entry on the stack was a typed object.  That just seemed like
the smallest impedance mismatch between forth and java, since 
you could have java.lang.Strings on the stack and what not. However,
the final code here approximates a typical forth situation where
everything on the stack is an integer, which you can treat as a number
or as a pointer to memory.

I've heard of some implementations using a big array to simulate 
memory and pointers.  I didn't do that.  Everything on my 
stack is a 64-bit long.  I used the high 32-bits as a "segment" selector
(think x86 hardware), and the low 32-bits as the offset.  So memory
access goes through a primitive mmu of sorts, to find the array
associtated with the pointer.  See `PointerManagment.java` for details.
 It works well, and lets me allocate space as I go.


