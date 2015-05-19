% vim: set filetype=prolog :

% ASCII mandelbrot ***************************************

% this initial version is deficient in that it hardcodes
% numbers at the top level (scene/0 and row/4), and does
% not yet let the user do the interactive exploration that
% the other implementations allow.


% compute the norm^2 *************************************
norm2(X,Y,Norm) :- Norm is X*X+Y*Y.


% compute the mandelbrot value at a point in space *******
mandel(X,Y,MandelVal) :- mhelp(X,Y,X,Y,0'~,MandelVal).

mhelp(_,_,_,_,32,32) :- !. % give up at the space
mhelp(X,Y,_,_,Ch,Ch) :-    % return current character 
  norm2(X,Y,N2),           % when the norm is > 4
  N2 > 4,
  !. 
mhelp(X,Y,Px,Py,Ch,MandelVal) :-   % otherwise, iterate
  Ch1 is Ch - 1,
  X1 is X*X - Y*Y + Px,
  Y1 is X*Y*2 + Py, !,
  mhelp(X1,Y1,Px,Py,Ch1,MandelVal).


% generate a row of mandelbrot points ********************
row(Y,ULX,XScale,Row) :-
  findall(MV, 
          ( between(1,72,Idx), 
            X is ULX + Idx*XScale, 
            mandel(X,Y,MV) ), 
         Row). 


% generate the mandelbrot scene **************************
scene :-
  forall(between(0,26,Idx),
    ( Y is 1 - 0.1*Idx,
      row(Y,-2,0.04,Row),
      format("~s~n",[Row]) )
  ).

main :- scene, halt.
