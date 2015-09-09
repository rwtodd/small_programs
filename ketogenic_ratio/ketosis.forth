\ vim: set filetype=forth :
\ Ketosis calculator
\ runs in gforth

\ storage for CARBS PROT FAT, and selector words... ***********
: FARRAY CREATE floats allot   DOES> swap floats + ;
3 FARRAY cpfData
: carbs 0 ; : prot 1 ; : fat 2 ;
: reset 0e carbs cpfData f!  0e prot cpfData f!  0e fat cpfData f! ;
reset

\ ability to tell the program what you've eaten **************
: ?>float  fdepth 0= IF swap s>f THEN ;
: eaten  ?>float cpfData dup f@ f+ f! ;

\ calculate the ketogenic ratio *******************************
: fetch  cpfData f@ ;
: krNUM  prot fetch 0.46e f*   fat fetch 0.9e f*  f+ ;
: krDEN  prot fetch 0.58e f*   fat fetch 0.1e f*  f+  carbs fetch f+ ;
: kr krNUM krDEN f/ ;

\ calculate the fat needed for a KR of 2 **********************
: 2kr:fat  carbs fetch 2e f*  prot fetch 0.7e f* f+ 0.7e f/ ;

\ report on the ketogenic ratio
: .ratio    cr cr cr 
   ." You ate " carbs fetch f. ." Carbs, " 
   prot fetch f. ." Protein, and " fat fetch f. ." fat." cr
   ." Ketogenic Ratio:   " kr f. cr 
   ." Fat for a KR of 2: " 2kr:fat fdup f. cr
   ." So you need to eat " fat fetch f- fdup fabs f. 
    f0>= IF ." more fat." ELSE ." less fat." THEN cr cr cr ;

