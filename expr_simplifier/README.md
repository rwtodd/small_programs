Expression Simplifier
=====================

I enjoyed [this post][1], which implemented an expression simplifier
in F#, Scala, Haskell, C++, and Julia.  I thought the C++ version,
using a boost library for variants, was especially ugly. So, I took
a stab at making a version based on a simple discriminated union.

I think my version is a bit more straight-forward, though it could
be that if I read the docs on the boost library I would be
more impressed with the blog version. I _did_ use an anonymous
struct inside the union, which is non-standard C++14. However,
it's allowed in C11 and all the major compilers seem to take it.

Still, my solution is far more fiddly than I'd like.  I think, 
in the end, the _right_ line of thinking for C++ is to
use inheritance and virtual functions, and forget trying to
emulate a pattern-matching feature that it just doesn't have. It's
nice that languages like scala have both options available to the
programmer.

For comparison, I did my own version of the scala solution, and 
it came out ridiculously close to the one in the blog post. I
guess there's not much room for variation once you settle on the
idea of case classes and pattern matching.


Both programs are here for posterity.

[1]: http://phdp.github.io/posts/2015-04-05-automated-reasoning.html "blog post"
