% vim: set filetype=prolog :

% pasc(Row,Col,Pasc)
% The pascal triangle value at Row,Col is Pasc
pasc(1,1,1) :- !.
pasc(Row,Col,Pasc) :-
  Row > 1,   Col > 0,   Col =< Row, !,
  Row1 is Row - 1,   Col1 is Col - 1,
  pasc(Row1,Col,P1),   pasc(Row1,Col1,P2),
  Pasc is P1 + P2.
pasc(_,_,0).

% print_tri(Size)
% print the triangle of size Size
print_tri(Size) :- 
  between(1,Size,Y), format("~n"), 
  between(1,Y,X),   pasc(Y,X,PVal),   format("~d ",PVal), 
  fail.

