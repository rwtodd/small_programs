fvariable x fvariable y
fvariable zx fvariable zy
fvariable initx  -2e initx f!
fvariable inity  -1e inity f!
fvariable incx   0.04e incx f!
fvariable incy   0.1e incy f!
24 constant ysize   80 constant xsize

: xs! fdup x f! zx f! ;    : ys! fdup y f! zy f! ;
: nextx x f@ incx f@ f+ xs! y f@ zy f! ;
: nexty y f@ incy f@ f+ ys! initx f@ xs! ;
: fsq fdup f* ;    : 2sq fover fsq fover fsq ;
: next zx f@ zy f@   2sq f- x f@ f+ zx f!   f* 2e f* y f@ f+ zy f! ;
: mag zx f@ fsq zy f@ fsq f+ fsqrt ;
: mandel cr inity f@ ys!  initx f@ xs!
  ysize 0 DO  xsize 0 DO  32 126 DO
    next mag 2e f>    I 32 = or   IF I emit LEAVE THEN
  -1 +LOOP   nextx LOOP  cr nexty LOOP ;
: cmds cr ." Zoom-(I)n  Zoom-(O)ut  (Q)uit " cr
          ." Use cursor keys to pan around. " cr ;
: f+! tuck f@ f+ swap f! ;
: f*! tuck f@ f* swap f! ;
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
       [CHAR] Q OF cr ." BYE!" bye ENDOF
      ENDCASE ;
: pan?  
      ekey>fkey IF
        CASE
          k-up    OF -4.0e shifty ENDOF
          k-down  OF  4.0e shifty ENDOF
          k-left  OF -4.0e shiftx ENDOF
          k-right OF  4.0e shiftx ENDOF
        ENDCASE
      ELSE drop THEN ;
: main
    page mandel cmds
    ekey ekey>char 
    IF zoom/quit? ELSE pan? THEN 
    recurse ;

main \ run the program!
