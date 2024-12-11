:- use_module(library(lists)).
:- use_module(library(between)).
:- use_module(library(system), [now/1]).

% Header
sight:-
    write('======================\n'),
    write('         SIGHT        \n'),
    write('======================\n').

% Initial Menu
initial_menu(1, 'Human vs. Human').
initial_menu(2, 'Human vs. Machine').
initial_menu(3, 'Machine vs. Machine').
read_initial_menu :-
    repeat,
    write_inital_menu,
    read(InitialMenuNumber),
    (   initial_menu(InitialMenuNumber, InitialMenuName)
    ->  write('You selected: '), write(InitialMenuName), nl, !, option(InitialMenuNumber)
    ;   write('Invalid Choice, try again'), nl, fail
    ).
write_inital_menu :-
    initial_menu(N, Name),
    write(N), write('. '), write(Name), nl,
    fail.
write_inital_menu.


% Initial Menu Options
option(1):-
    write('Player 1 Name'),
    read(Name1),
    asserta(name_of(player1, Name1)),
    write('Player 2 Name'),
    read(Name2),
    asserta(name_of(player2, Name2)),
    write('\n').
option(2):-
    write('Player 1 Name'),
    read(Name1),
    asserta(name_of(player1, Name1)),
    asserta(name_of(player2,'Machine')), !,
    set_difficulty(player2).
option(3):-
    asserta(name_of(player1, 'Machine1')),
    asserta(name_of(player2, 'Machine2')), !,
    set_difficulty(player1),
    set_difficulty(player2).

% Set Difficulty
difficulty(1, easy).
difficulty(2, normal).
difficulty(3, hard).
set_difficulty(Machine):-
    repeat,
    format('Select ~a difficulty:', [Machine]), nl,
    write_difficulty_list,
    read(DifficultyNumber),
    (   difficulty(DifficultyNumber, DifficultyName)
    -> write('You selected: '), write(DifficultyName), nl, !
    ;  write('Invalid choice, try again'), nl, fail
    ).
write_difficulty_list :-
    difficulty(N, Name),
    write(N), write('. '), write(Name), nl,
    fail.
write_difficulty_list.
    


% Choose White Pieces
choose_white_pieces :-
    name_of(player1, Name1),
    name_of(player2, Name2),
    write('Who will have the white pieces and go first?'), nl,
    format('1. ~w~n', [Name1]),
    format('2. ~w~n', [Name2]),
    repeat,
    read(Choice),
    (   (Choice == 1, asserta(starts_first(player1)), format('~w will go first.~n', [Name1]), !) ;
        (Choice == 2, asserta(starts_first(player2)), format('~w will go first.~n', [Name2]), !) ;
        (write('Invalid choice, try again.'), nl, fail)
    ).



settings([Board, Player, [], 0]) :-
    sight,
    read_initial_menu,
    choose_white_pieces.

play :-
    settings(GameState).

board(5,[
    [empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty]
]).

symbol(empty, '|  |'):-!.
symbol(black_one, '|B1|'):-!.
symbol(black_two, '|B2|'):-!.
symbol(black_three, '|B3|'):-!.
symbol(black_four, '|B4|'):-!.
symbol(black_five, '|B5|'):-!.
symbol(white_one, '|W1|'):-!.
symbol(white_two, '|W2|'):-!.
symbol(white_three, '|W3|'):-!.
symbol(white_four, '|W4|'):-!.
symbol(white_five, '|W5|'):-!.

position(Board, Col, Row, Piece) :-
    nth1(Row, Board, RowList),
    nth1(Col, RowList, Piece).
 
set_position(Board, Col, Row, Piece, NewBoard) :-
    nth1(Row, Board, OldRow),
    replace_in_list(OldRow, Col, Piece, NewRow),
    replace_in_list(Board, Row, NewRow, NewBoard).

replace_in_list(List, Index, Elem, NewList) :-
    nth1(Index, List, _, Rest),
    nth1(Index, NewList, Elem, Rest).

get_symbol(Piece, Symbol) :-
    symbol(Piece, Symbol).

% Display the entire game board with numbered rows and diagonal lines.
display_game(Board) :-
    nl, write('      1    2    3    4    5'), nl,
    write('   +----+----+----+----+----+'), nl,
    display_rows_with_diagonals(Board, 1).

% Displays rows along with their connecting diagonal lines.
display_rows_with_diagonals([], _).
display_rows_with_diagonals([Row], RowNumber) :-
    format(' ~d |', [RowNumber]),
    display_row(Row),
    nl,
    write('   +------------------------+'), nl.
display_rows_with_diagonals([Row1, Row2 | Rest], RowNumber) :-
    format(' ~d |', [RowNumber]),
    display_row(Row1),
    nl,
    generate_diagonal_line(RowNumber),
    display_rows_with_diagonals([Row2 | Rest], RowNumber + 1).

% Display a single row's content, with proper formatting for the last symbol.
display_row([Piece]) :-
    get_symbol(Piece, Symbol),
    write(Symbol),  
    write('|').     
display_row([Piece | Rest]) :-
    get_symbol(Piece, Symbol),
    write(Symbol),  
    write('-'),     
    display_row(Rest).  

% Generate the diagonal lines between rows.
generate_diagonal_line(RowNumber) :-
    (   RowNumber mod 2 =:= 1 -> write('   +----\\----/----\\----/----+')
    ;   write('   +----/----\\----/----\\----+')
    ),
    nl.
