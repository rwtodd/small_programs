\ vim: set filetype=forth :
\ hexagram explorer             2013 -- 11 -- 17              Richard W. Todd


\ base data ...  ************************************************************
CREATE (num->hex) 63 c,  0 c, 17 c, 34 c, 23 c, 58 c,  2 c, 16 c,
                  55 c, 59 c,  7 c, 56 c, 61 c, 47 c,  4 c,  8 c,
                  25 c, 38 c,  3 c, 48 c, 41 c, 37 c, 32 c,  1 c,
                  57 c, 39 c, 33 c, 30 c, 18 c, 45 c, 28 c, 14 c,
                  60 c, 15 c, 40 c,  5 c, 53 c, 43 c, 20 c, 10 c,
                  35 c, 49 c, 31 c, 62 c, 24 c,  6 c, 26 c, 22 c,
                  29 c, 46 c,  9 c, 36 c, 52 c, 11 c, 13 c, 44 c,
                  54 c, 27 c, 50 c, 19 c, 51 c, 12 c, 21 c, 42 c,
: num->hex ( n -- hex ) 1- (num->hex) + c@ ;
: slow:hex->num ( hex -- n ) (num->hex) dup 64 + swap DO 
     I c@ over = IF drop I (num->hex) - 1+ UNLOOP EXIT THEN 
  LOOP ; 
: makeinv CREATE 64 0 DO I slow:hex->num c, LOOP ;
makeinv (hex->num) 
: hex->num ( hex -- n ) (hex->num) + c@ ;
: array$"  POSTPONE c" POSTPONE over POSTPONE ! 
           POSTPONE cell POSTPONE + ;  IMMEDIATE
: array CREATE cells allot DOES> swap cells + ;

require hextext.fs

: title ( n -- addr u ) 1- (hexagrams) @ count ;
: trigram ( hex -- addr u ) 7 and (trigrams) @ count ;
: description ( n -- line2{addr,cnt} line1{addr,cnt} ) 
  1- 1 lshift (descriptions) dup cell + @ count rot  @ count ;

\ back list ... *************************************************************
create (hex)Stack 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c,  
variable (hex)SP   0 (hex)SP !
: hextop ( -- n ) (hex)SP @ (hex)Stack + c@ ;
: inc%10 ( n -- n' )  1+ dup 10 <> and ;
: dec%10 ( n -- n' )  dup 0= 10 and + 1-  ;
: ->hextop ( -- ) (hex)SP @ inc%10 (hex)SP ! ;
: <-hextop ( -- ) (hex)SP @ dec%10 (hex)SP ! ;
: ->hextop! ( n -- ) (hex)SP @ inc%10 dup (hex)SP !  (hex)Stack + c! ; 

\ drawing a hexagram... ***************************************************** 
variable (hex)X variable (hex)Y
: at ( x y  -- ) (hex)Y !   (hex)X ! ;
: type72 ( addr count -- ) tuck type  72 swap -  spaces ;
: .title ( n -- ) (hex)X @ (hex)Y @ 13 + at-xy  
  title  type72 ;
: .description ( n -- ) (hex)X @ (hex)Y @ 15 + at-xy
  description type72    (hex)X @ (hex)Y @ 16 + at-xy 
  type72 ; 
: .line  ( 1-or-0 n -- ) 
  (hex)X @   12 rot 2 * - (hex)Y @ +   at-xy   
  IF ." ############" ELSE ." #####  #####" THEN ;
: nxt1  ( n -- n' 1-or-0 ) dup 1 rshift swap   1 and  ;
: .hexlines ( hex -- )  
   nxt1 1 .line   nxt1 2 .line   nxt1 3 .line   
   nxt1 4 .line   nxt1 5 .line   nxt1 6 .line  drop ;
: .trigrams ( hex -- )
   dup      trigram (hex)X @ 14 + (hex)Y @ 8 + at-xy type
   3 rshift trigram (hex)X @ 14 + (hex)Y @ 2 + at-xy type ;
: .hexagram ( n -- ) dup .title dup .description  num->hex dup .hexlines .trigrams ;
: incnum ( n -- n' ) dup 64 <> and 1+ ; 
: decnum ( n -- n' ) 1- dup 0= 64 and + ; 

\ transformations ***********************************************************
: (innerhex) ( hex - hex' )  dup 1 lshift [ 32 16 8 + + ] LITERAL and  swap 
                                 1 rshift [  4  2 1 + + ] LITERAL and + ;
: innerhex ( n -- n' ) num->hex  (innerhex) hex->num ;
: inverthex ( n -- n' ) num->hex invert 63 and hex->num ;
: (changeline) ( hex line  -- hex' ) 1 swap 1- lshift xor ;
: changeline ( n line -- n' ) swap num->hex swap (changeline)  hex->num ;

\ user interface ************************************************************
: question 0 24 at-xy ;    : erase 72 spaces ;
: get-num ( min max -- n ) pad 10 accept   pad swap s>number d>s  
   min max ;
: go-menu ( -- n ) 
   question ." Which hexagram (1 - 64)? "    1 64 get-num    question erase ;
: change-menu ( n -- n ) 
   question ." Which line (1 - 6)? "  1 6 get-num changeline  question erase ;
: .instructions ( -- ) 0 21 at-xy
." (p)rev/(n)ext  (g)o-to  (c)hange-line  (i)nner  in(v)ert" cr
."          (b)ack/(f)orward               (q)uit" ;
: upkey key toupper ;
: mainloop dup   3 3 at hextop .hexagram   question upkey  
   CASE [CHAR] N  OF hextop incnum      ->hextop! ENDOF 
        [CHAR] P  OF hextop decnum      ->hextop! ENDOF
        [CHAR] G  OF go-menu            ->hextop! ENDOF
        [CHAR] C  OF hextop change-menu ->hextop! ENDOF
        [CHAR] I  OF hextop innerhex    ->hextop! ENDOF
        [CHAR] V  OF hextop inverthex   ->hextop! ENDOF
        [CHAR] B  OF <-hextop                     ENDOF
        [CHAR] F  OF ->hextop                     ENDOF
        [CHAR] Q  OF EXIT                         ENDOF 
   ENDCASE recurse ;
: main init(hexagrams) init(descriptions) init(trigrams)  
   page .instructions mainloop page ;

main bye
