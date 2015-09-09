\ vim: set filetype=forth :
\ Ketosis calculator
\ runs in gforth

\ storage for CARBS, PROT, FAT... *******************************
fvariable carbs    fvariable prot   fvariable  fat
prot constant protein \ let them say protein if they want
: reset 0e carbs f!   0e prot f!   0e fat f! ;
reset

\ ability to tell the program what you've eaten *****************
: ?>float  fdepth 0= IF swap s>f THEN ;  ( eaten can take float or int )
: eaten  ?>float dup f@ f+ f! ;

\ calculate the ketogenic ratio *********************************
: krNUM  0.46e prot f@ f*   0.9e fat f@ f*   f+ ;
: krDEN  0.58e prot f@ f*   0.1e fat f@ f*   f+   carbs f@  f+ ;
: kr krNUM krDEN f/ ;

\ calculate the fat needed for a KR of 2 ************************
: 2kr:fat  2e carbs f@ f*   0.7e prot f@ f*   f+   0.7e f/ ;

\ report on the ketogenic ratio
: .ratio    cr cr cr 
   ." You ate " carbs f@ f. ." Carbs, " 
   prot f@ f. ." Protein, and " fat f@ f. ." fat." cr
   ." Ketogenic Ratio:   " kr f. cr 
   ." Fat for a KR of 2: " 2kr:fat fdup f. cr
   ." So you need to eat " fat f@ f- fdup fabs f. 
    f0>= IF ." more fat." ELSE ." less fat." THEN cr cr cr ;

