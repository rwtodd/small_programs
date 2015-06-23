\p
--------------------------------------------------
   Base library for my FORTH implementation.
   Richard Todd
   Started: 2013 - 02 - 11
--------------------------------------------------

\p We'll need to create the rest of the default
parsing words next...

: CHAR       -1 FILE-CHAR ;
: SKIP       -1 SWAP FILE-SKIP ;
: PARSE      -1 SWAP FILE-PARSE ;
: WORD       -1 SWAP FILE-WORD ;
: FILE-LINE ( n -- addr # )    10 FILE-PARSE ;
: LINE      (   -- addr # ) -1 10 FILE-PARSE ;


32         CONSTANT BL   \ HEX 20 is ASCII for space

: [char]   char postpone literal ; IMMEDIATE   


\ -------------------------------------------------------------
\ Conditionals... IF ELSE THEN
: IF   
   POSTPONE __JZ__    \ skipc this if FALSE (0)
   __COMPLEN__        \ tell ELSE and ENDIF where to put jump dist
   POSTPONE 0         \ placeholder to be replaced
   ; IMMEDIATE
   
\ loc -- loc'
: ELSE 
   POSTPONE __JMP__     \ end of THEN part, jump over ELSE...
   __COMPLEN__          \ tell FI where to fixup the jump
   POSTPONE 0           \ placeholder for }-FI
   SWAP DUP             \ now fixup the if JZ to jump here for ELSE
   __COMPLEN__ SWAP -   \ distance...
   SWAP __COMPL__       \ dist loc compile
  ; IMMEDIATE
   
\ loc --  ... the location of the fixum address is on the stack 
: THEN
   DUP                  \ location of fixup
   __COMPLEN__ SWAP -   \ distance...
   SWAP __COMPL__       \ dist loc compile
   ; IMMEDIATE

: CASE-FUNC      0 ( if-then count ) 
  ; IMMEDIATE

: __EQ_TEST__    over = ;                   
: CASE  ( -- )   ['] __EQ_TEST__  POSTPONE CASE-FUNC 
  ; IMMEDIATE


\ leaves the tested value on the stack for use... unlike plain OF
: ?OF  ( case-test #cases -- if-stuff case-test #cases ) 
  1 +                       \ new case  
  POSTPONE IF         -ROT  \ push back the if-test data
  ; IMMEDIATE

: OF  ( case-test #cases --  if-stuff case-test #cases )
  OVER COMPILE,  
  1 +
  POSTPONE IF         -ROT
  POSTPONE DROP
  ; IMMEDIATE

: ENDOF  ( if-stuff case-test #cases -- if-stuff case-test #cases )
  ROT       POSTPONE ELSE       -ROT
  ; IMMEDIATE

: END-CASE    \ doesn't drop the thing we're switching on...
  ROT POSTPONE THEN 
  1 - ?DUP IF    tail-call END-CASE    THEN
  DROP  \ forget the function we're calling
  ; IMMEDIATE 
    
: ENDCASE    \ drop the top of the stack
  POSTPONE DROP    POSTPONE END-CASE 
  ; IMMEDIATE

: matching-char  ( ch -- ch' )
  CASE
    [char] {   OF   [char] }   ENDOF
    [char] <   OF   [char] >   ENDOF
    [char] [   OF   [char] ]   ENDOF
    [char] (   OF   [char] )   ENDOF     
  END-CASE ; \ let the original pass through if we didn't match anything...

: LEN$ ( $ -- cnt )  w@ ;

: __compilehelp       \ stopchar -- 
  BL WORD             \ sc wrd cnt
  dup 1 = IF
     swap dup c@      \ sc cnt wrd ch 
     3 pick   = IF    \ sc cnt wrd
        drop drop drop EXIT
     THEN
     swap             \ sc wrd cnt
  THEN
  \ it's not the stop word, so do some magic!
  FIND DROP POSTPONE,
  tail-call __compilehelp
  ;

: POSTPONE-> 
   bl skip char matching-char  \ figure out where to stop parsing...
   __compilehelp
 ; IMMEDIATE

: DO                             ( end st -- )
    POSTPONE-> / 2DUP <> IF /
    POSTPONE-> / 2>R /
    __COMPLEN__ 
  ; IMMEDIATE
    
    
: +LOOP
    POSTPONE-> / R> + DUP R@            /  ( st' st' end  RS: end )
    POSTPONE-> / <   IF                 /
    POSTPONE-> /      >R                /  (  --  RS: end st' ) 
    SWAP  \ put our number back in front of IF's number
    POSTPONE-> /   __JMP__              /  (  jump to the top )
    __COMPLEN__ - COMPILE,
    POSTPONE-> / THEN                   /
    POSTPONE-> / R>DROP DROP            /  (  cleanup... )
    POSTPONE-> / ELSE 2DROP THEN        /  (  did we do the loop at all? )
    ; IMMEDIATE

: LOOP   POSTPONE-> | 1 +LOOP | ; IMMEDIATE

: -LOOP
    POSTPONE-> / R> SWAP - DUP R@       /  \  st st end  RS: end
    POSTPONE-> / >   IF                 /
    POSTPONE-> /        >R              /  \  --  RS: end st' 
    swap                                  \ put our number back in front of IF's number
    POSTPONE-> /   __JMP__              /  \  jump to the top
    __COMPLEN__ - COMPILE,
    POSTPONE-> / THEN                   /
    POSTPONE-> / R>DROP DROP            /  \  cleanup...
    POSTPONE-> / ELSE 2DROP THEN        /  (  did we do the loop at all? )
    ; IMMEDIATE

: UNLOOP POSTPONE-> / 2R>DROP  / ; IMMEDIATE

\ cheap begin/until in terms of a DO until true causes it is loop 
: BEGIN  POSTPONE-> / -1 0 DO       / ; IMMEDIATE
: UNTIL  POSTPONE-> / TRUE  = NEGATE -LOOP    / ; IMMEDIATE
: WHILE  POSTPONE-> / FALSE = NEGATE -LOOP    / ; IMMEDIATE
: AGAIN  POSTPONE-> / 0 -LOOP                 / ; IMMEDIATE

\ copy some memory and store the len to make it a counted string ($)
: COPY-AS-$  ( src dest cnt -- $ )
    SWAP 2DUP w!    2 +     SWAP COPY     2 -  ;

\p ALLOC allocates space from the dictionary space, and borrow just
makes sure there is enough space to write to, though it is not
truly allocated.                   

: ALLOC      DICT BUF-ALLOC  ;
: BORROW     DICT BUF-BORROW ;

\ tmp versions are for interactive use, leave answer on stack
: __strborrow   DUP 2 + BORROW SWAP COPY-AS-$ ;
: __stralloc    DUP 2 + ALLOC  SWAP COPY-AS-$ ;
: PARSE-STRING  BL SKIP   CHAR MATCHING-CHAR  PARSE ;  \ leave in TIB area...
: $->      PARSE-STRING     __stralloc POSTPONE LITERAL ; IMMEDIATE
: "        [CHAR] " PARSE   __stralloc POSTPONE LITERAL ; IMMEDIATE
: tmp$->   PARSE-STRING     __strborrow                 ; 

\ FIXME hide the names __strborrow and __stralloc at some point

: CONSTANT$->     PARSE-STRING __stralloc CONSTANT ;

CONSTANT$-> {
} $CR

: COUNT ( $     -- addr cnt ) DUP 2 + SWAP w@ ;
: .$->  ( . str --          ) POSTPONE-> / $-> COUNT TYPE / ; IMMEDIATE 
: ."    POSTPONE-> / " COUNT TYPE / ; IMMEDIATE

: VARIABLE ( "name" -- )  1 CELLS ALLOC  CREATE DROP ;

: EVALUATE ( addr cnt -- )  mem-file file-evaluate ( FIXME file-close) ; 
