\ vim: set filetype=forth :
\ hexagram explorer             2013 -- 11 -- 17              Richard W. Todd


\ base data ...  ************************************************************
:noname DOES> 1- + c@ ;
CREATE num->hex EXECUTE
                  63 c,  0 c, 17 c, 34 c, 23 c, 58 c,  2 c, 16 c,
                  55 c, 59 c,  7 c, 56 c, 61 c, 47 c,  4 c,  8 c,
                  25 c, 38 c,  3 c, 48 c, 41 c, 37 c, 32 c,  1 c,
                  57 c, 39 c, 33 c, 30 c, 18 c, 45 c, 28 c, 14 c,
                  60 c, 15 c, 40 c,  5 c, 53 c, 43 c, 20 c, 10 c,
                  35 c, 49 c, 31 c, 62 c, 24 c,  6 c, 26 c, 22 c,
                  29 c, 46 c,  9 c, 36 c, 52 c, 11 c, 13 c, 44 c,
                  54 c, 27 c, 50 c, 19 c, 51 c, 12 c, 21 c, 42 c,
: slow:hex->num ( hex -- n ) 
   65 1 DO   I num->hex  over =  IF drop I LEAVE THEN   LOOP ;
: makeinv CREATE 64 0 DO I slow:hex->num c, LOOP DOES> + c@ ;
makeinv hex->num 

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
3 CONSTANT (hex)X    3 CONSTANT (hex)Y
: type-n ( addr count len -- ) -rot  tuck type  -  spaces ;
: .title ( n -- ) (hex)X  [ (hex)Y 13 + ]L  at-xy  
  dup s>d <<# bl hold [char] . hold # # #> type #>> 
  title  68 type-n ;
: .description ( n -- ) description  
    (hex)X  [ (hex)Y 15 + ]L  at-xy  72 type-n 
    (hex)X  [ (hex)Y 16 + ]L  at-xy  72 type-n ; 
: .line  ( hex y -- ) (hex)X over at-xy   
    over  1 and  IF ." ############" ELSE ." #####  #####" THEN ;
: nxt  ( hex y -- hex' y' ) 2 - >r 1 rshift r> ;
: .hexlines ( hex -- )  [ 10 (hex)Y + ]L
     .line nxt   .line nxt   .line nxt 
     .line nxt   .line nxt   .line 2drop ;
: .trigrams ( hex -- )
   dup      trigram [ (hex)X 14 + ]L [ (hex)Y 8 + ]L  at-xy type
   3 rshift trigram [ (hex)X 14 + ]L [ (hex)Y 2 + ]L  at-xy type ;
: .hexagram ( n -- ) dup .title   dup .description  
            num->hex dup .hexlines    .trigrams ;
: incnum ( n -- n' ) dup 64 <> and 1+ ; 
: decnum ( n -- n' ) 1- dup 0= 64 and + ; 

\ transformations ***********************************************************
: (innerhex) ( hex - hex' )  dup 1 lshift [ 32 16 8 + + ]L and  swap 
                                 1 rshift [  4  2 1 + + ]L and + ;
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
: mainloop dup  hextop .hexagram   question upkey  
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
: main init(trigrams) init(descriptions) 
   page .instructions mainloop page ;

main bye
