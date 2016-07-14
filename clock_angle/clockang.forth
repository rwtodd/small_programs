\ Clock Angle -- angle between the hour and minute hand on a 12-hr clock

: hourAng ( hour min -- angle )  s>f 60e f/  12 mod s>f f+ 360e 12e f/ f* ;
: minAng  ( min -- angle ) s>f 360e 60e f/ f* ;
: clockAng ( hour min -- angle ) dup minAng hourAng f- fabs ;

12 00 clockAng f. cr
2 00 clockAng f. cr
6 00 clockAng f. cr
5 24 clockAng f. cr
2 20 clockAng f. cr

