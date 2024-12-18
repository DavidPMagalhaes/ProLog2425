:- use_module(settings).
:- use_module(board).
:- use_module(game_logic).

:- initialization(main).

main :-
    sight,
    settings(GameState),
    game_loop(GameState).
