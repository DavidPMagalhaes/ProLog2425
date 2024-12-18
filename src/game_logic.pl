:- module(game_logic, [game_loop/1]).

:- use_module(board).
:- use_module(settings).

% Main game loop
game_loop([Board, Player, MoveHistory, TurnCount]) :-
    display_game(Board),
    (   game_over(Board, Player) ->
        declare_winner(Player);
        valid_moves(Board, Player, Moves),
        play_turn(Board, Player, Moves, MoveHistory, TurnCount, NewBoard, NewMoveHistory),
        next_player(Player, NextPlayer),
        NewTurnCount is TurnCount + 1,
        game_loop([NewBoard, NextPlayer, NewMoveHistory, NewTurnCount])
    ).

% Check if the game is over
game_over(Board, Player) :-
    \+ valid_moves(Board, Player, _).

% Declare the winner
declare_winner(Player) :-
    next_player(Player, Winner),
    format('~w wins the game!~n', [Winner]).

% Determine all valid moves for the current player
valid_moves(Board, Player, Moves) :-
    (   player_has_stack(Board, Player) ->
        find_stack_moves(Board, Player, Moves);
        find_placement_moves(Board, Moves)
    ).

% Check if a player has a stack on the board
player_has_stack(Board, Player) :-
    member(Stack, [Player-one, Player-two, Player-three, Player-four, Player-five]),
    member(Row, Board),
    member(Stack, Row).

% Find all valid type 1 (placement) moves
find_placement_moves(Board, Moves) :-
    findall([Col, Row], position(Board, Col, Row, empty), Moves).

% Find all valid type 2 (stack movement) moves
find_stack_moves(Board, Player, Moves) :-
    findall([FromCol, FromRow, ToCol, ToRow],
            (   position(Board, FromCol, FromRow, Stack),
                stack_belongs_to_player(Stack, Player),
                adjacent_space(Board, FromCol, FromRow, ToCol, ToRow)
            ),
            Moves).

% Check if a stack belongs to the player
stack_belongs_to_player(Stack, Player) :-
    sub_atom(Stack, 0, _, _, Player).

% Check for adjacent empty spaces
adjacent_space(Board, FromCol, FromRow, ToCol, ToRow) :-
    between(-1, 1, DC),
    between(-1, 1, DR),
    DC \= 0 ; DR \= 0,  % Skip the same position
    ToCol is FromCol + DC,
    ToRow is FromRow + DR,
    position(Board, ToCol, ToRow, empty).

% Play a turn: ask the player for input and update the game state
play_turn(Board, Player, Moves, MoveHistory, TurnCount, NewBoard, NewMoveHistory) :-
    format('~w, it is your turn.~n', [Player]),
    format('Valid moves: ~w~n', [Moves]),
    prompt_move(Player, Moves, SelectedMove),
    apply_move(Board, SelectedMove, Player, NewBoard),
    append(MoveHistory, [SelectedMove], NewMoveHistory).

% Prompt the player to choose a move
prompt_move(Player, Moves, SelectedMove) :-
    repeat,
    write('Enter your move (format: [Col, Row] or [FromCol, FromRow, ToCol, ToRow]): '),
    read(SelectedMove),
    format('You entered: ~w~n', [SelectedMove]), % Debugging line
    (   member(SelectedMove, Moves) ->
        true;
        write('Invalid move, try again.'), nl, fail
    ).


% Apply the selected move to the board
apply_move(Board, [Col, Row], Player, NewBoard) :-  % Type 1 move
    stack_for_player(Player, Stack),
    set_position(Board, Col, Row, Stack, NewBoard).
apply_move(Board, [FromCol, FromRow, ToCol, ToRow], _, NewBoard) :-  % Type 2 move
    position(Board, FromCol, FromRow, Stack),
    set_position(Board, FromCol, FromRow, empty, TempBoard),
    set_position(TempBoard, ToCol, ToRow, Stack, NewBoard).

% Get the stack corresponding to the player
stack_for_player(player1, Stack) :-
    member(Stack, [white_one, white_two, white_three, white_four, white_five]).
stack_for_player(player2, Stack) :-
    member(Stack, [black_one, black_two, black_three, black_four, black_five]).



% Determine the next player
next_player(player1, player2).
next_player(player2, player1).
