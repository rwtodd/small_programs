\ vim: set filetype=forth :

\ Number puzzle: http://programmingpraxis.com/2015/07/21/a-number-puzzle/

\ low-level bit tracking
VARIABLE bits  0 bits !  \ which digits we've chosen
VARIABLE #bits 0 #bits ! \ how many digits we've chosen
: >bit ( n -- bit ) 1 swap lshift ;
: btest ( n -- t/f ) >bit  bits @  AND  0= ;
: bset ( n -- ) >bit         bits @  OR   bits ! ;
: bclr ( n -- ) >bit invert  bits @  AND  bits ! ;
: next-avail ( n -- n' ) BEGIN 1+ dup btest UNTIL ;

\ backtrack backs up a digit... add-digit pushes forward
: backtrack  ( n -- n' ) 10 /  -1 #bits +! ;
: add-digit  ( n -- n' )
    10 *  0 next-avail  dup bset  +  1 #bits +! ;

\ the next three words search for the next number
\ to test while we are not divisible, backtracking
\ if necessary
: next-digit ( n0 digit -- n' failed? )
   next-avail dup 10 < IF dup bset + false
                     ELSE drop true THEN ;
: split-digit ( n -- n0 digit )
   dup 10 MOD  dup bclr  tuck -  swap ;
: next-num  ( n -- n' )
   split-digit next-digit IF backtrack recurse THEN ;

\ here is the main loop, trying numbers until
\ we get the solution.
: divisible? ( n -- t/f ) #bits @  MOD   0= ;
: done? ( -- t/f ) #bits @  9 =  ;
: try-nums ( n -- n' t/f )
   dup divisible?  IF done? IF true ELSE add-digit false THEN
                 ELSE next-num false THEN ;

\ on 32-bit gforth, the final answer is too big for a
\ cell, so we have to go to doubles...
\ well, there's no d*, but there is d+ and d2*,
\ so we use the formula:  (x*2*2 + x)*2 = x*10
: d10* ( d -- d*10 ) 2dup d2* d2* d+ d2* ;

: solve ( -- )
   0 add-digit BEGIN try-nums UNTIL 0 d10* d. ;
