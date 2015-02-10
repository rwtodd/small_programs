fvariable x fvariable y
fvariable initx  -2e initx f!
fvariable inity  -1e inity f!
fvariable incx   0.04e incx f!
fvariable incy   0.1e incy f!
34 constant ysize   80 constant xsize

\ **********************************************************************
\ Some macros to inline the low-level stuff
: f+! ( sym F: n -- ) POSTPONE dup POSTPONE f@ 
                      POSTPONE f+ POSTPONE f! ; immediate
: f*! ( F: n sym -- ) POSTPONE dup POSTPONE f@ 
                      POSTPONE f* POSTPONE f! ; immediate
: -frot ( F: a b c -- F: c a b ) POSTPONE frot POSTPONE frot ; immediate
: fsq   POSTPONE fdup POSTPONE f* ; immediate 
: 2fsq  POSTPONE fover POSTPONE fsq POSTPONE fover POSTPONE fsq ; immediate


\ **********************************************************************
\ Compute a mandelbrot set value at a point
\ **********************************************************************
: next ( F: x y -- F: x' y' ) 2fsq f- x f@ f+ -frot   f* f2*  y f@ f+ ;
: mag ( F: x y -- x y mag ) 2fsq f+ fsqrt ;
: mandel-point ( F: r F: i -- n )
    33 126 DO
       next mag 2e f>   IF fdrop fdrop I UNLOOP EXIT THEN
    -1 +LOOP 
    fdrop fdrop 32 ;


\ **********************************************************************
\ Loop over the screen, creating and emitting mandel-points ...
\ **********************************************************************
: init>xy ( -- ) inity f@ y f! initx f@ x f! ;
: nextx ( -- ) incx f@ x f+!  ;
: nexty ( -- ) incy f@ y f+!   initx f@ x f! ;
: mandel ( -- ) cr init>xy
  ysize 0 DO  xsize 0 DO  
    x f@ y f@ mandel-point emit
  nextx LOOP  cr nexty LOOP ;


\ **********************************************************************
\ Controls to pan and zoom around the field
\ **********************************************************************
: cmds cr ." Zoom-(I)n  Zoom-(O)ut  (Q)uit " cr
          ." Use cursor keys to pan around. " cr ;
: shiftx incx f@ f* initx f+! ;
: shifty incy f@ f* inity f+! ;
: view-adjust fdup xsize s>f 0.25e f* f* shiftx
                   ysize s>f 0.25e f* f* shifty ;
: scale-adjust fdup incy f*! incx f*! ;
: zoom-in 1e view-adjust  0.5e scale-adjust ; 
: zoom-out 2.0e scale-adjust -1e view-adjust ;
: zoom/quit? 
      toupper
      CASE
       [CHAR] I OF zoom-in ENDOF
       [CHAR] O OF zoom-out ENDOF
       [CHAR] Q OF page cr ." BYE!" bye ENDOF
      ENDCASE ;
: pan?  
      ekey>fkey IF
        CASE
          k-up    OF -6.0e shifty ENDOF
          k-down  OF  6.0e shifty ENDOF
          k-left  OF -6.0e shiftx ENDOF
          k-right OF  6.0e shiftx ENDOF
        ENDCASE
      ELSE drop THEN ;

\ **********************************************************************
\ The main program 
\ **********************************************************************
: main-loop
    0 0 at-xy mandel 
    ekey ekey>char 
    IF zoom/quit? ELSE pan? THEN ;
: main page   0 ysize 1+ at-xy cmds    begin main-loop  again ; 

main \ run the program!
