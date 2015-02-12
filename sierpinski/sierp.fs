64 constant SIZE   CHAR * constant '*'
create cur-line bl c, SIZE chars allot bl c, 
create new-line SIZE chars allot

new-line SIZE blank   '*' new-line SIZE 2/ chars + c!  \ init

: .new ( -- ) new-line SIZE type cr ;
: cur=*? ( i -- f ) chars cur-line + c@ '*' = ;
: parents ( i -- f1 f2 ) dup cur=*?  swap 2 + cur=*?  ;
: new! ( i f -- ) [ '*' bl - ] LITERAL and bl + swap chars new-line + c! ;
: sierp-line ( -- ) SIZE 0 DO  i dup parents xor new!  LOOP ;
: new>cur ( -- ) new-line cur-line 1+ SIZE chars move ;
: main ( -- ) SIZE 2/ 0 DO .new new>cur sierp-line LOOP ;
main bye
