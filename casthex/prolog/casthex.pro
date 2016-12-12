:- initialization(main).

% ~~~~ visual representation of hexagram lines
disp1(0'7,'  -------').
disp1(0'8,'  --   --').

disp2(0'6,'  --   --   -->   -------').
disp2(0'7,'  -------         -------').
disp2(0'8,'  --   --         --   --').
disp2(0'9,'  -------   -->   --   --').

writenl(Line) :- write(Line), nl.

display_hex( Lines, Hex, Hex ) :-
  hex_name(Hex, HName),
  format("~a~2n", [ HName ] ),
  maplist(disp1, Lines, DispLines),
  maplist(writenl, DispLines).
display_hex( Lines, H1, H2 ) :-
  H1 \= H2,
  hex_name(H1, H1Name),
  hex_name(H2, H2Name),
  format("~a~n - Changing to -->~n~a~2n", [ H1Name, H2Name ] ),
  maplist(disp2, Lines, DispLines),
  maplist(writenl, DispLines).

display_casting( Lines ) :-
  reverse( Lines, Senil ),
  format( "Casting: <~s>~2n", [ Senil ] ).

% ~~~~ coins method
tossed_coins( C ) :- 
  random( 0, 2, C1 ), random( 0, 2, C2 ), random( 0, 2, C3 ),
  C is 0'6 + C1 + C2 + C3.
coins_method( Lines ) :- 
  length( Lines, 6 ), 
  maplist( tossed_coins, Lines ).

% ~~~~ static method, no moving lines
static_choice( C ) :- 
  random( 0, 2, C1 ), 
  C is 0'7 + C1.
static_method( Lines ) :- 
  length( Lines, 6 ), 
  maplist( static_choice, Lines ).

% ~~~~ yarrow stalk method
stalks( S ) :- 
  random( 0, 16, R ),
  ( R = 0               -> S = 0'6 ;
    between( 1,  5, R ) -> S = 0'7 ; 
    between( 6, 12, R ) -> S = 0'7 ; 
                           S = 0'9 ).
stalks_method( Lines ) :- 
  length( Lines, 6 ), 
  maplist( stalks, Lines ).

% ~~~~ user-input method 
interpret_arg( '-coins', Lines ) :-  !, coins_method( Lines ).
interpret_arg( '-stalks', Lines ) :- !,  stalks_method( Lines ).
interpret_arg( '-static', Lines ) :- !, static_method( Lines ).
interpret_arg( UI, Lines ) :- 
  atom_codes( UI, LRev ),
  reverse( LRev, Lines ),
  length( Lines, 6 ).
interpret_arg( _, _ ) :-
  format( "Usage: casthex (-coins|-stalks|-static|<casting>)~n", [] ),
  format( "  -coins   3-coins method~n  -stalks  yarrow stalks method~n", [] ),
  format( "  -static  a random hexagram~n", [] ),
  format( "  <casting> 6 digits from the set {6,7,8,9}~n", [] ),
  nl, nl, fail.
  
% ~~~~ interpret the command line... default to coins method
gen_lines( 1, Lines ) :- coins_method( Lines ).
gen_lines( 2, Lines ) :- 
  argument_value( 1, L ), 
  interpret_arg( L, Lines ).

% ~~~~ convert hexagram lines to a single binary number
line_vals( 0'6, 0, 1 ).
line_vals( 0'7, 1, 1 ).
line_vals( 0'8, 0, 0 ).
line_vals( 0'9, 1, 0 ).
hex_numbers_help( [ ], H1, H2, H1, H2 ).
hex_numbers_help( [L|Ls], H10, H20, H1, H2 ) :-
   line_vals( L, V1, V2 ),
   H11 is (H10 << 1) \/ V1,
   H21 is (H20 << 1) \/ V2,
   hex_numbers_help( Ls, H11, H21, H1, H2 ).
hex_numbers( Lines, H1, H2 ) :-
   hex_numbers_help( Lines, 0, 0, H1, H2 ).
  
% ~~~~ the program's entry point
main :- 
  randomize,
  argument_counter( Argc ),
  gen_lines( Argc, Lines ),
  display_casting( Lines ),
  hex_numbers( Lines, H1, H2 ),
  display_hex( Lines, H1, H2 ),
  nl.
