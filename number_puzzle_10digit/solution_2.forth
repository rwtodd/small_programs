\ vim: set filetype=forth :

\ Number puzzle: http://programmingpraxis.com/2015/07/21/a-number-puzzle/

\ Adapted from Paul Rubin's solution 
\   https://groups.google.com/d/msg/comp.lang.forth/qQtSWWql3U8/2EZ8UFaLAQAJ 

: .bit ( i -- n ) 1 swap lshift ;          \ bits are numbered from 0
: clear-bit ( n i -- n ) .bit invert and ; \ clear bit i in n
: test-bit ( n i -- flag ) .bit and 0<> ;  \ true if bit i is 1
: append-digit ( n i -- n ) swap 10 * + ;

: num { r n s -- n }
    s 0= if n else
        10 1 do
            n i append-digit
            s i test-bit
            over r mod 0= and if
                r 1+ swap  s i clear-bit recurse
            else drop
            then
        loop
    then ;

: d10*  2dup d2* d2* d+ d2* ;
: solve 1 0 $3fe num 0 d10* d. ;

