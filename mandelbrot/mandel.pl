% vim: set filetype=prolog :

:- use_module( library( readutil ) ).

% ASCII mandelbrot ***************************************

% use l, r, u, d to pan, and i/o to zoom In or Out.
% q to quit


% compute the norm^2 *************************************
norm2( X, Y, Norm ) :- Norm is X*X+Y*Y.


% compute the mandelbrot value at a point in space *******
mandel( X, Y, MandelVal ) :- 
  mhelp( X, Y, X, Y, 0'~, MandelVal ).

mhelp( _, _, _, _, 0' , 0' ) :- !. % give up at the space
mhelp( X ,Y, _, _, Ch, Ch ) :-    % return current character 
  norm2( X, Y, N2 ),              % when the norm is > 4
  N2 > 4,
  !. 
mhelp( X, Y, Px, Py, Ch, MandelVal ) :-   % otherwise, iterate
  Ch1 is Ch - 1,
  X1 is X*X - Y*Y + Px,
  Y1 is X*Y*2 + Py, !,
  mhelp( X1, Y1, Px, Py, Ch1, MandelVal ).


% generate a row of mandelbrot points ********************
row( Y, ULX, XScale, Row ) :-
  findall( MV, 
           ( between( 1, 72, Idx ), 
             X is ULX + Idx*XScale, 
             mandel( X, Y, MV ) 
           ), 
           Row 
         ). 


% generate the mandelbrot scene **************************
scene( Frame ) :-
  Frame = frame( ULX, ULY, XScale, YScale ),
  forall( between( 0, 26, Idx ),
          ( Y is ULY - YScale*Idx,
            row( Y, ULX, XScale, Row ),
            format( "~s~n", [ Row ] ) 
          )
        ).

% get input from the user, and act upon it ***************
parse_commands( [], LastFrame, LastFrame ).
parse_commands( [ A | Rest ], Frame, LastFrame ) :-
  run_command( A, Frame, Frame1 ), !,
  parse_commands( Rest, Frame1, LastFrame ).

run_command( 0'l, Frame, NewFrame ) :-           % go left
  Frame = frame( ULX, ULY, XScale, YScale ),
  ULX1 is ULX - 6 * XScale,
  NewFrame = frame( ULX1, ULY, XScale, YScale ).
run_command( 0'r, Frame, NewFrame ) :-           % go right
  Frame = frame( ULX, ULY, XScale, YScale ),
  ULX1 is ULX + 6 * XScale,
  NewFrame = frame( ULX1, ULY, XScale, YScale ).
run_command( 0'u, Frame, NewFrame ) :-           % go up
  Frame = frame( ULX, ULY, XScale, YScale ),
  ULY1 is ULY + 4 * YScale,
  NewFrame = frame( ULX, ULY1, XScale, YScale ).
run_command( 0'd, Frame, NewFrame ) :-           % go down
  Frame = frame( ULX, ULY, XScale, YScale ),
  ULY1 is ULY - 4 * YScale,
  NewFrame = frame( ULX, ULY1, XScale, YScale ).
run_command( 0'i, Frame, NewFrame ) :-           % zoom in
  Frame = frame( ULX, ULY, XScale, YScale ),
  ULX1 is ULX + (72/4) * XScale,
  ULY1 is ULY - (26/4) * YScale,
  XScale1 is XScale / 2,
  YScale1 is YScale / 2,
  NewFrame = frame( ULX1, ULY1, XScale1, YScale1 ).
run_command( 0'o, Frame, NewFrame ) :-           % zoom out
  Frame = frame( ULX, ULY, XScale, YScale ),
  XScale1 is XScale * 2,
  YScale1 is YScale * 2,
  ULX1 is ULX - (72/4) * XScale1,
  ULY1 is ULY + (26/4) * YScale1,
  NewFrame = frame( ULX1, ULY1, XScale1, YScale1 ).
run_command( 0'q, _, done ).                     % quit
run_command( _ , Frame, Frame ).                 % ignore unknowns

main_loop( done ) :- !.
main_loop( Frame ) :-
  format( "~n~n~n~n~n~n~n~n~n" ),
  scene( Frame ),
  read_line_to_codes( current_input, Commands ),
  parse_commands( Commands, Frame, NewFrame ), !,
  main_loop( NewFrame ). 

main :- 
  main_loop( frame( -2.0, +1.0, 0.04, 0.1 ) ), 
  halt.
