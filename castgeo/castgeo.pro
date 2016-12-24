:- initialization(main).

% ~~~~ visual representation of geomantic lines
disp(1,"  *  ").
disp(2,"*   *").
dispAll( Gs, Strs ) :- maplist( disp, Gs, Strs ).
spaces(N, Sp) :- length(Sp, N), maplist(=(32),Sp).

writenl(Line) :- format("~s~n",[Line]).

% ~~~~ SWI-prolog has append/2, but for GnuProlog I 
%      had to create my own:
concat( [], [] ).
concat( [L], L ).
concat( [[]|Ls], CC ) :- concat(Ls,CC).
concat( [[L1|Ls]|Ls2], [L1|CC] ) :- concat( [Ls|Ls2], CC ).

% ~~~~ Example: alternate( [1,2,3], 0, [1,0,2,0,3])
alternate([], _ , []).
alternate([L], _, [L]) :- !.
alternate([L1|Ls], Middle, [L1,Middle|Rest]) :- alternate(Ls, Middle, Rest).

% ~~~~ concatenate all the strings in Lst, with initial Sp1 spaces, and 
%      Sp2 spaces between each item in Lst
intersperse(Sp1, Sp2, Lst, Answer) :-
  spaces(Sp1, First),
  spaces(Sp2, Middle),
  alternate(Lst, Middle, Alts),
  concat([First|Alts], Answer).

% ~~~~ display a list of geomantic figures, with Sp1 initial spaces
%      and Sp2 spaces between the figures.
display_casting(Sp1, Sp2, Lines ) :-
  transpose( Lines, TLines ),
  maplist( reverse, TLines, Revved ),
  maplist( dispAll, Revved, RevStr ),
  maplist( intersperse(Sp1, Sp2), RevStr, Spaced ),  
  maplist( writenl, Spaced ),
  nl.

% ~~~~ a random geomantic figure
random_geo( [C1,C2,C3,C4] ) :-
  random( 1, 3, C1 ), random( 1, 3, C2 ),
  random( 1, 3, C3 ), random( 1, 3, C4 ).
  
mothers( Ms ) :- 
  length( Ms, 4 ),
  maplist( random_geo, Ms ).

% ~~~~ transpose, used both for display and for generating
%      daughters from the mothers
transpose( [], [] ).
transpose( Ls, [Hs|T2] ) :-
  splitAll(Ls, Hs, Tls ),
  transpose(Tls, T2).
transpose( _, [] ).

splitAll( [], [], [] ).
splitAll( [[H1|T1]|Ls], [H1|Hs], [T1|Ts] ) :-
  splitAll(Ls, Hs, Ts).
  
daughters( Ms , Ds ) :- transpose( Ms, Ds ).

% ~~~~ combine two figures... needed to make 
%      nieces, witnesses, and judges from two parents
addUp( Same, Same, 2 ).
addUp( N1,   N2,   1 ) :- N1 \= N2.
combine( F1, F2, Comb ) :- maplist( addUp, F1, F2, Comb ).

combPair( [], [] ).
combPair( [ F1, F2 | Fs ], [ N1 | Ns ] ) :-
  combine( F1, F2, N1 ),
  combPair( Fs, Ns ).

nieces( Fs, Ns ) :- combPair( Fs, Ns ).
witnesses( Ns, Ws ) :- combPair( Ns, Ws ).
judge( Ws, J ) :- combPair( Ws, [J] ).

% ~~~~ the program's entry point
main :- 
  randomize,
  mothers( Moms ),
  daughters( Moms, Daughters ),
  append( Moms, Daughters, Line1 ),
  nieces( Line1, Nieces ),
  witnesses( Nieces, Witnesses ),
  judge( Witnesses, Judge ),
  display_casting(2,5,Line1),
  display_casting(7,15,Nieces),
  display_casting(17,35,Witnesses),
  display_casting(37,0,[Judge]),
  nl.
