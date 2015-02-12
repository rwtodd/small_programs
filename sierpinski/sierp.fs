64 constant SIZE   CHAR * constant '*'
: char-array create does> swap chars + ;
char-array cur[]  bl c, SIZE chars allot bl c, 
char-array new[]  SIZE chars allot            

0 new[] SIZE blank   '*' SIZE 2/ new[] c!  \ initialize

: .new ( -- ) 0 new[] SIZE type cr ;
: cur=*? ( i -- f ) cur[] c@   '*' = ;
: parents ( i -- f1 f2 ) dup cur=*?  swap 2 + cur=*?  ;
: >new[] ( i f -- ) [ '*' bl - ] LITERAL and bl + swap new[] c! ;
: sierp-line ( -- ) SIZE 0 DO  i dup parents xor >new[]  LOOP ;
: new>cur ( -- ) 0 new[]   1 cur[]   SIZE chars move ;
: main ( -- ) SIZE 2/ 0 DO .new new>cur sierp-line LOOP ;

main bye
