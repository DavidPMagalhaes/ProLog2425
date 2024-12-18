:- module(game_logic, [game_loop/1]).

:- use_module(board).
:- use_module(settings).
:- use_module(library(between)).  % Import the 'between' predicate

% Main game loop
game_loop([Board, Player, MoveHistory, TurnCount]) :-
    display_game(Board),
    (   game_over(Board, Player) ->
        declare_winner(Player);
        valid_moves(Board, Player, Moves),
        play_turn(Board, Player, Moves, MoveHistory, TurnCount, NewBoard, NewMoveHistory),
        update_sight_lines(NewBoard, Player, UpdatedBoard), % Update sight lines here
        next_player(Player, NextPlayer),
        NewTurnCount is TurnCount + 1,
        game_loop([UpdatedBoard, NextPlayer, NewMoveHistory, NewTurnCount])
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

% Check if a player has a stack on the board (only consider stacks with more than 1 piece)
player_has_stack(Board, Player) :-
    stack_for_player(Player, Stack),
    member(Row, Board),
    member(Stack, Row),
    Stack \= white_one,  % Exclude single pieces (W1, B1)
    Stack \= black_one.

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

% Check if a stack belongs to the player (only consider stacks with more than 1 piece)
stack_belongs_to_player(Stack, Player) :-
    stack_for_player(Player, Stack),
    Stack \= white_one,  % Exclude single pieces (W1, B1)
    Stack \= black_one.

% Check for adjacent empty spaces
adjacent_space(Board, FromCol, FromRow, ToCol, ToRow) :-
    (   between(-1, 1, DC),  % DC (Delta Column) ranges from -1 to 1
        between(-1, 1, DR),  % DR (Delta Row) ranges from -1 to 1
        (DC \= 0; DR \= 0),  % Ensure it's not the same position
        ToCol is FromCol + DC,  % New column (ToCol) calculated
        ToRow is FromRow + DR,  % New row (ToRow) calculated
        position(Board, ToCol, ToRow, empty)  % Check if the position is empty
    ).

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
apply_move(Board, [Col, Row], Player, NewBoard) :-  % Type 1 move (place a piece)
    stack_for_player(Player, Stack),
    set_position(Board, Col, Row, Stack, NewBoard).

apply_move(Board, [FromCol, FromRow, ToCol, ToRow], Player, NewBoard) :-  % Type 2 move (move a stack)
    position(Board, FromCol, FromRow, Stack),
    set_position(Board, FromCol, FromRow, empty, TempBoard), % Remove from old position
    set_position(TempBoard, ToCol, ToRow, Stack, NewBoard). % Place on new position

% Get the stack corresponding to the player (this ensures the correct symbols are used)
stack_for_player(player1, Stack) :-
    member(Stack, [white_one, white_two, white_three, white_four, white_five]).
stack_for_player(player2, Stack) :-
    member(Stack, [black_one, black_two, black_three, black_four, black_five]).

% Determine the next player
next_player(player1, player2).
next_player(player2, player1).

% Update the sight lines after a move: Check up, down, left, right, and diagonals
update_sight_lines(Board, Player, UpdatedBoard) :-
    findall([Col, Row], position(Board, Col, Row, Player), Pieces),
    promote_pieces(Board, Player, Pieces, UpdatedBoard).

% Promote pieces in sight lines
promote_pieces(Board, Player, [], Board).
promote_pieces(Board, Player, [[Col, Row] | Rest], NewBoard) :-
    position(Board, Col, Row, Piece),
    promote_piece(Piece, NewPiece),
    set_position(Board, Col, Row, NewPiece, TempBoard),
    promote_pieces(TempBoard, Player, Rest, NewBoard).

% Promote a piece (e.g., white_one -> white_two)
promote_piece(white_one, white_two).
promote_piece(white_two, white_three).
promote_piece(white_three, white_four).
promote_piece(white_four, white_five).
promote_piece(black_one, black_two).
promote_piece(black_two, black_three).
promote_piece(black_three, black_four).
promote_piece(black_four, black_five).
promote_piece(Piece, Piece).  % No promotion if it's already at the highest level
