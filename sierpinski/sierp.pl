% vim: set filetype=prolog :

size(64).

blanks(0,[]).
blanks(N,[32|Rest]) :- N > 0, N1 is N - 1, blanks(N1,Rest).

init_line(Line) :- size(Sz), Sz2 is Sz/2, blanks(Sz2,Bl), 
                   append(Bl,[0'*|Bl],Line).

next_line([Ch,_],[Ch]).
next_line([Ch1,Ch2,Ch1|Rest],[ 32|Out])  :- !, next_line([Ch2,Ch1|Rest],Out).
next_line([          _|Rest],[0'*|Out])  :- next_line(Rest,Out).

plines(1,Line) :- format(Line).
plines(N,Line) :- N > 1, N1 is N - 1,
                  format(Line), nl,
				  next_line([32|Line],Line1),
				  plines(N1,Line1).

main :- init_line(IL), size(Sz), Sz2 is Sz/2, plines(Sz2,IL), halt.
