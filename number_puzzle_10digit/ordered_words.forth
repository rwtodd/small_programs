\ vim: set filetype=forth : 
\ ordered words
\ http://programmingpraxis.com/2015/07/14/ordered-words/

: read2 ( addr -- c c ) dup c@ swap 1+ c@ ;
: ordered? ( addr len -- t/f )
    dup 2 < IF 2drop false EXIT THEN
    1- bounds DO I read2 > IF UNLOOP false EXIT THEN LOOP
    true ;

: larger? ( cnt -- t/f ) PAD @ > ;
: copy-word ( addr u )  dup PAD ! PAD cell+ swap cmove  ;

create line-buffer 256 allot 
: procLine ( u -- ) 
    line-buffer swap 2dup ordered? over larger? AND 
    IF copy-word ELSE 2drop THEN ;
: read-all ( -- ) 
   BEGIN line-buffer 255 stdin read-line throw 
   WHILE procLine 
   REPEAT drop ;

: .winner ( -- )  PAD cell+ PAD @ dup . type cr ;

: main ( -- ) cr read-all .winner ;

main bye
