:- module(board, [board/2, display_game/1, position/4, set_position/5, get_symbol/2]).
:- use_module(library(lists)).

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

