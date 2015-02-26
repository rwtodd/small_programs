\ vim: set filetype=forth :
\ ROBOTS -- like the BSD game

\ ********** helper functions that don't belong anywhere else ***
: bi@ ' dup COMPILE, POSTPONE >r COMPILE, POSTPONE r> ; immediate
: exit-program  page ." BYE!" bye ;
: get-num ( min max -- n ) pad 10 accept   pad swap s>number d>s  
   min max ;
\ ***************************************************************


\ ********** random.fs from gforth distribution *****************
Variable seed
$10450405 Constant generator
: rnd  ( -- n )  seed @ generator um* drop 1+ dup seed ! ;
: random ( n -- 0..n-1 )  rnd um* nip ;
\ ***************************************************************


\ ********** board outline **************************************
70 constant WIDTH       20 constant HEIGHT
: .c# ( ch n -- ) 0 DO dup emit LOOP drop ;
: .line ( side mid -- ) over emit   WIDTH .c#  emit cr ;
: .top ( -- ) [char] + [char] - .line ; 
: .side ( -- ) [char] | bl .line ;
: .board ( -- ) .top   HEIGHT 0 DO .side LOOP .top ; 
: board-xy ( x y -- ) bi@ 1+   at-xy ;
: in-bounds ( x y -- x' y' ) 0 MAX HEIGHT 1- MIN >r 
                             0 MAX WIDTH  1- MIN r> ;
: apply-movement ( x y dx dy -- x' y' )
     rot bi@ +  in-bounds ;
: random-pos ( -- x y ) WIDTH random HEIGHT random  ;
: status-line ( -- ) 0 HEIGHT 2 + at-xy ;
: marker-locations ( x y -- locs ) bi@ 1+ 
    >r  0 over [ HEIGHT 1+ ] literal
	0 r@  [ WIDTH 1+ ] literal r> ;
: markers ( x y -- ) marker-locations 
    at-xy [char] < emit  at-xy [char] > emit  
	at-xy [char] ^ emit  at-xy [char] v emit ;
: nomarkers ( x y -- ) marker-locations  
    at-xy [char] | emit  at-xy [char] | emit  
	at-xy [char] - emit  at-xy [char] - emit ;
                       
\ ***************************************************************


\ *********** our character *************************************
WIDTH 2/ VALUE EGO-x     HEIGHT 2/ VALUE EGO-Y 
char @ CONSTANT EGO-Char
: ego-xy ( -- ) EGO-x EGO-Y board-xy ; 
: move-ego ( x y ) TO EGO-Y TO EGO-X ; 
: .ego ( -- ) EGO-X EGO-Y markers  ego-xy EGO-Char emit ;
: erase-ego ( -- )  EGO-X EGO-Y nomarkers  ego-xy bl emit ;
: relmove-ego ( dx dy ) EGO-X EGO-Y apply-movement  move-ego ;  
: check-ego-death ( x y -- ) EGO-Y = swap EGO-X = and 
    IF ego-xy [CHAR] & emit   
       status-line ." YOU ARE DEAD!" key exit-program THEN ;
\ ***************************************************************


\ ********** enemies ********************************************
\ a robot fits in a 32-bit or bigger cell...
\    MSB  ..  LSB
\    YY XX ?? TT   ; YY/XX pos  ??=not used  TT = type

char R CONSTANT Type:ROBOT 
char o CONSTANT Type:HOLE
char x CONSTANT Type:DEAD

: en-pos ( e -- x y ) dup 16 rshift 255 and   
                     swap 24 rshift 255 and ;
: en-type ( e -- tt ) 255 and ;
: robot?  ( e -- f ) en-type Type:ROBOT = ;
: new-enemy ( type x y -- e ) 8 lshift or 16 lshift or ;
: enemy-xy ( e -- ) en-pos board-xy ;
: erase-enemy ( e -- ) enemy-xy bl emit ;
: .enemy ( e -- ) dup enemy-xy en-type emit ;
: kill-enemy ( e -- e' ) [ 255 invert ] LITERAL and Type:DEAD or ;
\ ***************************************************************

     
\ ********** sorted enemy array *********************************
8 VALUE #enemies      40 constant MAX-ENEMIES
CREATE enemies MAX-ENEMIES cells allot
: in-bounds? ( addr --  f ) \  are we inside the array still?
  enemies cell+ >= ; 
: grab2 ( addr -- addr@ addr-1@ ) dup @ swap 1 cells - @ ;
: swap-adjacent ( addr -- addr-1 )
        dup dup grab2 rot ! swap 1 cells - tuck !  ;
: sort-enemies ( -- ) 
  enemies cell+ #enemies 1- cells bounds DO 
     I BEGIN dup in-bounds? WHILE dup grab2 < WHILE 
     swap-adjacent REPEAT THEN drop
  cell +LOOP ;
: do-enemies ( exec -- ) enemies #enemies cells bounds 
     DO I over EXECUTE cell +LOOP drop ; 
: map-enemies ( exec -- ) enemies #enemies cells bounds 
     DO I @ over EXECUTE I ! cell +LOOP drop ; 
\ ***************************************************************
     

\ *********** enemy movement ************************************
: only-robots ( e -- e ) dup robot? invert IF r> drop THEN ;
: point-at ( c _ -- _ dc ) rot - sgn ;
: ego-direction ( x y -- dx dy ) EGO-X point-at  EGO-Y point-at ; 
: +-? ( n -- n' ) 100 random  dup 94 > IF 2drop 3 random 1- ELSE 
                                  89 > IF  drop 0 THEN THEN ;
: random-movement ( dx dy -- dx' dy' )  bi@ +-? ;
: accost ( e -- e' ) Type:ROBOT swap   en-pos 2dup ego-direction 
    random-movement  apply-movement   new-enemy ;
\ ***************************************************************


\ *********** User Input *****************************************
: proc-fkey CASE
    k-up    OF  0 -1 ENDOF
    k-down  OF  0  1 ENDOF
    k-left  OF -1  0 ENDOF
    k-right OF  1  0 ENDOF
    0 0 rot \ default 
  ENDCASE erase-ego relmove-ego .ego ;
: proc-char toupper CASE
    [char] Q OF   exit-program ENDOF  
    [char] + OF   erase-ego  random-pos move-ego .ego   ENDOF
  ENDCASE ;
: get-input ekey ekey>char 
    IF proc-char 
  ELSE ekey>fkey IF proc-fkey 
               ELSE drop THEN THEN ;
\ ***************************************************************


\ *********** Collision Detection *******************************
: en-location-bits [ 65535 invert ] literal and ;
: new-death ( eaddr -- ) 
     dup @ kill-enemy over !    cell+  
     dup @ kill-enemy over !    @ enemy-xy Type:DEAD emit ;
: detect-collisions 
    enemies @ en-pos check-ego-death
    enemies #enemies 1- cells bounds DO
       I @ en-location-bits   I cell+ @ en-location-bits 
       = IF I new-death THEN 
       I cell+ @ en-pos check-ego-death 
    cell +LOOP ;
\ ***************************************************************

utime xor seed ! 
: near-center? ( x y -- f ) 
    [ HEIGHT 2/ dup 3 + swap 2 - ] LITERAL LITERAL within >r 
    [ WIDTH  2/ dup 3 + swap 2 - ] LITERAL LITERAL within r> and ;
: off-center ( -- x y )
    begin random-pos 2dup near-center? while 2drop repeat ; 
: random-enemies ( #h #r ) 
   2dup + TO #enemies
   enemies swap 0 DO 
      Type:ROBOT off-center new-enemy over ! cell+
   LOOP 
   swap 0 DO
      Type:HOLE off-center new-enemy over ! cell+
   LOOP drop ;

: move-enemy ( e -- e' ) only-robots 
    accost  dup .enemy ;  
: erase-robot ( eaddr --  ) 
    @ dup robot? IF erase-enemy ELSE drop THEN ;
: erase-all ( -- ) ['] erase-robot do-enemies ;

: .enemy @ .enemy ;
: init-draw-enemies ( -- ) ['] .enemy do-enemies ;

: loop  get-input    ['] erase-robot do-enemies 
        ['] move-enemy map-enemies  
        sort-enemies    detect-collisions ;

: clear-status status-line 72 spaces ;
: question" POSTPONE clear-status POSTPONE status-line 
            POSTPONE ." ; immediate
: main ( -- )    
    page .board 
    question" ROBOTS! How many holes in the ground? " 
    1 20 get-num 
    question" ROBOTS! How many robots attacking? " 
    1 over MAX-ENEMIES - negate get-num
    random-enemies  
    clear-status .ego init-draw-enemies 
    BEGIN status-line loop AGAIN ; 

main
