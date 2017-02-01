\ ---------------------------- \
 \  C A S T  G E O              \
  \  Cast a geomantic shield.    \
   \  (c) 2017 Richard Todd       \
    \ ---------------------------- \


\ ********** random.fs from gforth distribution *****************
Variable seed
$10450405 Constant generator
: rnd  ( -- n )  seed @ generator um* drop 1+ dup seed ! ;
: random ( n -- 0..n-1 )  rnd um* nip ;
\ ***************************************************************


\ ********** beginning of actual program ************************
VARIABLE #figs                 ( how many figures in the row? )
CREATE figs 8 4 * CHARS ALLOT  ( get room for a row of 8 figs )

: floc ( row col -- addr ) 4 * + figs + ;

: .dots ( 0|1 -- ) IF ."   *  " ELSE ." *   *" THEN ;

: .row { isp msp -- }         
  4 0 DO
    isp spaces
    I 0 floc c@ .dots 
    #figs @ 1 ?DO   msp spaces    J I floc c@ .dots   LOOP
    cr
  LOOP ;

: line1 ( -- )
  8 #figs !
  4 7 DO   ( J = which fig )
    4 0 DO ( I = which row )
       2 random dup
       I      J     floc c!  ( mom      )
       7 J -  3 I - floc c!  ( daughter )
    LOOP
  -1 +LOOP ;

: merge ( -- ) 
  #figs @   2 /   dup   #figs !
  0 DO     ( J = merged col )
    4 0 DO ( I = row        )
       I J 2 * 2dup   floc c@   -rot 1+ floc c@   xor
       I J floc c!
    LOOP 
  LOOP ;
  
utime xor seed ! 
cr cr
line1    2  5 .row  cr ( moms & daughters )
merge    7 15 .row  cr ( nieces           )
merge   17 35 .row  cr ( witnesses        )
merge   37  0 .row  cr ( judge            )
bye
