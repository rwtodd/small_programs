Number Puzzle
=============

A problem I saw on [c.l.f][1]:

> Find a 10-digit number, with all digits unique, 
> such that the first n digits of the number are 
> divisible by n. For instance, in the 3-digit number 
> 345, the 1-digit prefix, 3, is divisible by 1, the 
> 2-digit prefix, 34, is divisible by 2, and the 3-digit 
> prefix, 345, is divisible by 3.

My first solution takes care not to check divisibility unless all
the digits are unique, and takes care to quit searching as
soon as it finds an answer. 

But, someone else posted a much more elegant-looking answer
which doesn't stop the search, and always checks divisibility.
The program is much simpler because of that (and because of the
use of locals).  Also, the main routine only leaves a response 
on the stack one time out of many recursive invocations, which
is something that's hard to do most languages.  I liked it so 
much I adapted it to my 32-bit gforth and saved it as solution 2.

A Bonus
=======

The [entry][1] on programming praxis mentioned [this other challenge][2],
so I decided to do it as well.  You just run it like so:

```
gforth ordered_words.forth < wordlist.txt 
```

... and it will give the largest ordered word, as well as 
printing out the length for convenience.

[1]: https://groups.google.com/d/topic/comp.lang.forth/qQtSWWql3U8/discussion

