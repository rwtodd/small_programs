\ vim: set filetype=forth :

\ Number puzzle: http://programmingpraxis.com/2015/07/21/a-number-puzzle/

\ low-level bit tracking
0 VALUE bits     \ which digits we've chosen
0 VALUE #digits  \ how many digits we've chosen
: >bit ( n -- bit ) 1 swap lshift ;
: bset ( n -- ) >bit         bits OR   TO bits ;
: bclr ( n -- ) >bit invert  bits AND  TO bits ;
: next-avail ( n -- n' ) 
    BEGIN 1+ dup  >bit bits AND 0=  UNTIL ;

\ backtrack backs up a digit... add-digit pushes forward
: backtrack  ( n -- n' ) 
    10 /    dup 10 MOD bclr    #digits 1- TO #digits ;
: add-digit  ( n -- n' )
    dup 10 MOD bset   10 *  0 next-avail +  
    #digits 1+ TO #digits ;

\ the next two words search for the next unique number
: split ( n -- n0 digit ) dup 10 MOD  tuck - swap ;  
: next-num  ( n -- n' )
    split next-avail 10 MOD tuck + swap 0= 
    IF backtrack recurse THEN ;

\ The main loop body adds a digit when
\ the guess is divisible, and goes to the next
\ unique-digit number when it is not divisible.
: divisible? ( n -- t/f ) #digits MOD 0= ;
: unless-done: ( magic ) #digits 8 =  IF true rdrop THEN ;
: try-nums ( n -- n' t/f )
   dup divisible? 
   IF unless-done: add-digit ELSE next-num THEN false ;

: solve ( -- )
   0 add-digit BEGIN try-nums UNTIL 10 m* d. ;
