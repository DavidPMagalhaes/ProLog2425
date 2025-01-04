:- module(settings, [settings/1, sight/0]).
:- use_module(game_logic).
:- use_module(board).

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
    retractall(name_of(player1, _)), % Remove quaisquer fatos anteriores de player1
    asserta(name_of(player1, Name1)),
    write('Player 2 Name'),
    read(Name2), 
    retractall(name_of(player2, _)), % Remove quaisquer fatos anteriores de player2
    asserta(name_of(player2, Name2)),
    write('\n'),
    choose_white_pieces,
    board(5, Board), % Criação do tabuleiro inicial
    game_loop([Board, StartingPlayer, [], 0]).

option(2):-
    write('Player 1 Name'),
    read(Name1),
    asserta(name_of(player1, Name1)),
    asserta(name_of(player2,'Machine')), !,
    set_difficulty(player2),
    board(5, Board), % Criação do tabuleiro inicial
    game_loop_randplayer([Board, StartingPlayer, [], 0]).
option(3):-
    asserta(name_of(player1, 'Machine1')),
    asserta(name_of(player2, 'Machine2')), !,
    set_difficulty(player1),
    set_difficulty(player2),
    board(5, Board),
    game_loop_rand([Board, StartingPlayer, [], 0]).



:- dynamic name_of/2.

% Inicialize com valores padrão para evitar erros
name_of(player1, player1).
name_of(player2, player2).

% Set Difficulty
difficulty(1, easy).
difficulty(2, hard).
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
    (   Choice == 1 -> 
        retractall(starts_first(_)), % Remove fatos antigos
        asserta(starts_first(player1)), 
        format('~w will go first.~n', [Name1]), !;
        Choice == 2 -> 
        retractall(starts_first(_)), % Remove fatos antigos
        asserta(starts_first(player2)), 
        format('~w will go first.~n', [Name2]), !;
        write('Invalid choice, try again.'), nl, fail
    ).

settings([Board, Player, [], 0]) :-
    read_initial_menu.
    
