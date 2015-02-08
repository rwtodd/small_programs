fvariable x fvariable y 
fvariable zx fvariable zy 

: xs! fdup x f! zx f! ;    : ys! fdup y f! zy f! ; 
: nextx x f@ 0.04e f+ xs! y f@ zy f! ; 
: nexty cr y f@ 0.1e f+ ys!  -2e xs! ; 
: fsq fdup f* ;    : 2sq fover fsq fover fsq ; 
: next zx f@ zy f@   2sq f- x f@ f+ zx f!   f* 2e f* y f@ f+ zy f! ; 
: mag zx f@ fsq zy f@ fsq f+ fsqrt ; 
: mandel cr -1e ys! -2e xs! 
  21 0 DO  76 0 DO  32 126 DO 
    next mag 2e f>    I 32 =   or
    IF I emit LEAVE THEN 
  -1 +LOOP   nextx LOOP   nexty LOOP ; 
