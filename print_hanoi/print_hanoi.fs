\ vim: set filetype=forth :

: a[ here 0 c, ; : ]a here over - 1- over c! ;
0 value disks : sz 2* 1+ ; : .c*n 0 DO dup emit LOOP drop ;
: center swap disks sz over - 2/ dup spaces -rot .c*n spaces ;
: .pr disks over c@ -  rot - negate dup 0<
   IF 2drop 1 [char] | ELSE  + 1+ c@ sz [char] - THEN center ;
: hanoi { a b c gap } a c@ b c@ c c@ + + dup TO disks  0
   DO I a .pr gap spaces  I b .pr  gap spaces I c .pr cr LOOP ;

a[ 4 c, 5 c, 6 c, 7 c, ]a a[  2 c, 3 c, ]a a[ 1 c, ]a 1 hanoi

