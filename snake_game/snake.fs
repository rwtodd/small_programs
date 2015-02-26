\ vim: set filetype=forth :
\ SNAKE -- like the old DOS game

\ ********** helper functions that don't belong anywhere else ***
: bi@ ' dup COMPILE, POSTPONE >r COMPILE, POSTPONE r> ; immediate
: exit-program  page ." BYE!" bye ;
\ ***************************************************************


\ ********** random.fs from gforth distribution *****************
Variable seed
$10450405 Constant generator
: rnd  ( -- n )  seed @ generator um* drop 1+ dup seed ! ;
: random ( n -- 0..n-1 )  rnd um* nip ;
\ ***************************************************************

\ ********** xy locations  **************************************
: >loc ( x y -- loc ) 8 lshift or ;
: loc> ( loc -- x y ) dup 255 and swap 8 rshift ;
: apply-movement ( dx dy loc -- loc' ) loc> rot bi@ + >loc ;
\ ***************************************************************

\ ********** board outline **************************************
70 constant WIDTH       20 constant HEIGHT
: .c# ( ch n -- ) 0 DO dup emit LOOP drop ;
: .line ( side mid -- ) over emit   WIDTH .c#  emit cr ;
: .top ( -- ) [char] + [char] - .line ; 
: .side ( -- ) [char] | bl .line ;
: .board ( -- ) .top   HEIGHT 0 DO .side LOOP .top ; 
: board-xy ( x y -- ) bi@ 1+   at-xy ;
: .ch ( loc ch -- ) swap loc> board-xy emit ;
: in-bounds? ( loc -- f ) loc> 0 HEIGHT within swap 
                               0 WIDTH  within and ;
: random-loc ( -- loc ) WIDTH random HEIGHT random >loc ;
\ ***************************************************************


\ ********** game status ****************************************
0 value score      \ how many apples have we eaten?
0 value apple-loc  \ where is the apple?
200 value speed    \ game speed

: status-line ( -- ) 0 HEIGHT 2 + at-xy ;
: clear-status ( -- ) status-line 72 spaces status-line ;
: .score clear-status ." SCORE: " score . 
    ." apple" score 1 <> IF [char] s emit THEN 
	."  eaten.  Use cursor keys to move." ;
: add(score) score 1+ to score  .score 
             speed 10 - 20 max to speed ;

: eat-keys  ekey? IF ekey drop recurse THEN ;
: die!die! clear-status ." YOU HAVE DIED! SCORE: " score .
    500 ms eat-keys key exit-program ; 
\ ***************************************************************


\ *********** our character *************************************
char @ CONSTANT Head-Char  char # CONSTANT Tail-Char
1 value DIR(X)  0 value DIR(Y)  \ which way we are moving

\ a circular buffer of tail locations
128 CONSTANT MAX-LENGTH
:noname DOES> swap 2* + ;
CREATE snake EXECUTE MAX-LENGTH 2* allot
0 value length  
variable head-idx
: idx++ ( idx -- idx' ) 1+ dup length <> and ;

: tail-pos@ ( idx -- loc ) snake uw@ ; 
: tail-pos! ( loc idx -- ) snake w! ;
: head-loc ( -- loc ) head-idx @ tail-pos@ ;

: snake-collision? ( loc -- f ) 
   length 0 DO I tail-pos@ over = 
      IF drop -1 UNLOOP EXIT THEN
   LOOP drop 0 ;
\ ***************************************************************


\ *********** initial status ************************************
: init(snake)
   3 to length   
   WIDTH 2/ HEIGHT 2/ >loc 
   dup Head-Char .ch 
   dup 1- dup Tail-Char .ch 
   dup 1- dup Tail-Char .ch
   0 tail-pos!   1 tail-pos!  2 tail-pos!
   2 head-idx ! 
   .score ;

: init(apple) 
  random-loc dup snake-collision? 
  IF drop recurse 
  ELSE dup to apple-loc [char] a .ch THEN ;

: init 
   HERE seed !   page .board   init(snake) init(apple) ; 
\ ***************************************************************


\ ************  movement  ****************************************
: crawl ( loc' -- )
   dup head-idx @  dup     \ ( loc' loc' idx idx ) 
   tail-pos@ Tail-Char .ch \ head to tail
   idx++ dup head-idx !    \ new head index
   dup tail-pos@ bl .ch    \ erase oldest
   tail-pos!               \ store new head loc
   Head-Char .ch ;         \ draw new head

: extend-tail ( idx -- ) 
   snake dup 2 +  over length snake - negate chars move 
   length 1+ to length ;

: grow ( loc ' -- )
   dup head-idx @  dup     \ ( loc' loc' idx idx ) 
   tail-pos@ Tail-Char .ch \ head to tail ( loc' loc' idx )
   idx++ dup head-idx !    \ new head index ( loc' loc' idx' )
   dup extend-tail 
   tail-pos!
   Head-Char .ch ;

: collision? ( loc -- f ) dup in-bounds? invert swap 
    snake-collision? or ;

: advance ( -- )
   DIR(X) DIR(Y) head-loc apply-movement ( loc' )
   dup collision? IF die!die! THEN 
   dup apple-loc =
   IF grow init(apple) add(score) ELSE crawl THEN ;
\ ***************************************************************


\ ************  user input ***************************************
: proc-fkey CASE
    k-up    OF  0 -1 ENDOF
	k-down  OF  0  1 ENDOF
	k-left  OF -1  0 ENDOF
	k-right OF  1  0 ENDOF
    DIR(X) DIR(Y) rot \ default
  ENDCASE to DIR(Y) to DIR(X) ;

: proc-char toupper CASE
   [char] Q OF exit-program ENDOF
  ENDCASE ;

: handle-keys 
    rnd drop ( exercise random generator )
    ekey ekey>fkey 
	IF proc-fkey 
  ELSE ekey>char IF proc-char 
	           ELSE drop THEN THEN ; 

: loop  speed ms 
        ekey? IF handle-keys THEN
	    advance
        recurse ;

: main  init loop ;
main
