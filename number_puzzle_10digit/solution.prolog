% vim: set filetype=prolog :

solve( SoFar, Len, Avail, Answer ) :-
  select( Digit, Avail, Left ),
  Next is 10*SoFar + Digit,
  0 is Next mod Len,
  Len1 is Len + 1,
  solve( Next, Len1, Left, Answer ).
solve( Answer, 11, [], Answer ).

solve( N ) :- solve( 0, 1, [1,2,3,4,5,6,7,8,9,0], N ).

