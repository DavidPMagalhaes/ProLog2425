:- use_module(settings). 
:- use_module(board).
:- use_module(game_logic).

play :-
    sight,
    settings(GameState).
