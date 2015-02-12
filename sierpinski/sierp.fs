64 constant SIZE   CHAR * constant '*'
create line[]  SIZE chars allot bl c, 

line[] SIZE blank   '*' SIZE 2/ chars line[] + c!  \ initialize

: .line[] ( -- ) line[] SIZE type cr ;
: =*? ( addr -- f ) c@   '*' = ;
: >char ( f f -- ch ) xor [ '*' bl - ] LITERAL and bl + ;
: init-flags ( -- f-1 f0 )  0  line[] =*? ; 
: sierp-line ( -- ) init-flags   line[] SIZE bounds DO  
       I 1 chars + =*?   rot over >char   I c! 
   LOOP 2drop ;
: main ( -- ) SIZE 2/ 0 DO .line[] sierp-line LOOP ;

main bye
