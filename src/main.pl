:- use_module(settings). 
:- use_module(board).
:- use_module(game_logic).

play :-
    sight,
    settings(GameState),
    board(5, Board), % Criação do tabuleiro inicial
    game_loop([Board, StartingPlayer, [], 0]).