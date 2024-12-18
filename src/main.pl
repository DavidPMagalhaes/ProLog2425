:- use_module(settings).
:- use_module(board).
:- use_module(game_logic).

play :-
    sight,
    settings(GameState), !,
    write('Initial Board: '), nl,
    board(5, Board),
    display_game(Board),
    game_loop(GameState).
