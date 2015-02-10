% vim: set filetype=prolog :
% hexagram explorer             2013 -- 11 -- 17                 Richard W. Todd
:- use_module(library(readutil)).
:- consult('iching.dat').

% screen stuff *****************************************************************
page :- write('\e[H\e[2J').
at(X,Y) :- 
  X1 is X + 1, 
  Y1 is Y + 1, 
  write('\e['), write(Y1), write(';'), 
  write(X1), write('H').

% trigrams *********************************************************************
trigram([0,0,0],'Earth     ').
trigram([0,0,1],'Thunder   ').
trigram([0,1,0],'Gorge     ').
trigram([0,1,1],'Lake      ').
trigram([1,0,0],'Mountain  ').
trigram([1,0,1],'Brightness').
trigram([1,1,0],'Wind      ').
trigram([1,1,1],'Heaven    ').

trigrams(Lines,Upper,Lower) :-
  length(ULines,3),
  length(LLines,3),
  append(ULines,LLines,Lines),
  trigram(ULines,Upper),
  trigram(LLines,Lower).

% hex manipulation *************************************************************
inner([_,L1,L2,L3,L4,_],[L1,L2,L3,L2,L3,L4]).

invert(Line,Inverted) :-  Inverted is 1 - Line.

invert_lines(Lines,Inverted) :- maplist(invert,Lines,Inverted).

change_line(Lines,Ln,Changed) :-
  RevLn is 7 - Ln,
  nth1(RevLn,Lines,Orig,Less),
  invert(Orig,New),
  nth1(RevLn,Changed,New,Less).

% i ching display **************************************************************
hexAt(3,2).
question :- at(0,20).
clear :- write('                                                                    ').
controls :- at(0,19), 
  write('(n)ext/(p)rev (f)orwd/(b)ack (g)oto (c)hange (i)nner  in(v)ert (q)uit').

disp_yinyang(0) :- write('#####  #####').
disp_yinyang(1) :- write('############').

disp_hex([],_,_).
disp_hex([X|Xs],LocX,LocY) :-
   at(LocX,LocY),
   disp_yinyang(X),
   NewY is LocY + 2,
   disp_hex(Xs,LocX,NewY).

disp_hex(Hex) :- hexAt(Bx,By), disp_hex(Hex,Bx,By).

disp_name(Num) :- 
   hexdesc(Num,Name,Line1,Line2), 
   hexAt(Bx,By), 
   NameY is By + 12, 
   at(Bx,NameY), clear, 
   at(Bx,NameY), writef("%s",[Name]),
   DescY is By + 14,
   at(Bx,DescY), clear, 
   at(Bx,DescY), writef("%s",[Line1]), 
   DescY2 is DescY + 1,
   at(Bx,DescY2), clear, 
   at(Bx,DescY2), writef("%s",[Line2]).

disp_trigrams(Hex) :-
   trigrams(Hex,Upper,Lower), 
   hexAt(Bx,By), 
   TriX is Bx + 14, 
   TriUY is By + 2, 
   TriLY is By + 8,
   at(TriX, TriUY), write(Upper), 
   at(TriX, TriLY), write(Lower).

hex_screen(Hist) :- 
   top_hex(Hist,Hex),
   top_num(Hist,Num),
   disp_hex(Hex), 
   disp_name(Num), 
   disp_trigrams(Hex).

% user input *******************************************************************
ask(Str) :- question, write(Str). 
doneAsk :- question, clear.
get_num(Min,Max,Num) :- read_line_to_codes(current_input, CD), 
  number_chars(Num,CD), Num >= Min, Num =< Max.

% history tracking *************************************************************
top_hex([hex(_,L)|_]-_,L).
top_num([hex(N,_)|_]-_,N).
init_hist( [hex(1,[1,1,1,1,1,1])]-[] ).

add_hist(B-_,Num,Hex,[New|B]-[]) :- 
  length(B,LenB), LenB < 10, 
  hexagram(Num,Hex),
  New = hex(Num,Hex).
add_hist(B-_,Num,Hex,[New|ShortB]-[]) :- 
  length(ShortB,9), 
  append(ShortB,_,B),
  hexagram(Num,Hex),
  New = hex(Num,Hex).

back_hist([B|Bs]-F,Bs-[B|F]).
back_hist([]-F,[]-F).

forw_hist(B-[F|Fs],[F|B]-Fs).
forw_hist(B-[],B-[]).

% program controls *************************************************************
incMod64(64,1).
incMod64(X,X1) :- X < 64, X1 is X + 1.

decMod64(1,64).
decMod64(X,X1) :- X > 1, X1 is X - 1.

action(n, Hist, NewH) :- top_num(Hist,Num), 
                         incMod64(Num, Num1), 
                         add_hist(Hist, Num1, _, NewH).

action(p, Hist, NewH) :- top_num(Hist,Num), 
                         decMod64(Num, Num1), 
                         add_hist(Hist, Num1, _,NewH).

action(q, _, _) :- halt.

action(i, Hist, NewH) :- top_hex(Hist,Hex), 
                         inner(Hex,NewHex), 
                         add_hist(Hist, _, NewHex, NewH).

action(v, Hist, NewH) :- top_hex(Hist,Hex), 
                         invert_lines(Hex, NewHex),
                         add_hist(Hist, _, NewHex,NewH).

action(g, Hist, NewH) :- ask('Go to which hexagram? '), 
                         get_num(1,64,NewNum),
                         add_hist(Hist, NewNum, _, NewH), 
                         doneAsk.

action(c, Hist, NewH) :- ask('Change which line? '), 
                         get_num(1,6,ChL),
                         top_hex(Hist,Hex), 
                         change_line(Hex,ChL,NewHex), 
                         add_hist(Hist, _, NewHex,NewH), 
                         doneAsk.

action(f, Hist, NewHist) :- forw_hist(Hist,NewHist).

action(b, Hist, NewHist) :- back_hist(Hist,NewHist).

action(_, Hist, Hist) :- doneAsk. % do nothing on unexpected input

% program main *****************************************************************
main_loop(Hist) :- 
   hex_screen(Hist), 
   question, 
   get_single_char(K), 
   char_code(Ch,K), 
   action(Ch,Hist,NewHist), 
   main_loop(NewHist).

% initialize the description database *****************************************
read_descriptions(65, _).
read_descriptions(N, In) :-
   N < 65,

   read_line_to_codes(In, Title),
   read_line_to_codes(In, Desc1),
   read_line_to_codes(In, Desc2),
   assertz(hexdesc(N,Title,Desc1,Desc2)),

   N1 is N + 1,
   read_descriptions(N1,In).

init_descriptions :-
   open('iching.txt',read,In),
   read_descriptions(1,In),
   close(In).

main :- 
   page, 
   controls, 
   init_descriptions, 
   init_hist(Hist), 
   main_loop(Hist).
