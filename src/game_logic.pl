
:- module(game_logic, [game_loop/1, valid_moves/3, apply_move/4, player_has_stack/2, find_placement_moves/2, find_stack_moves/3, promote_pieces/3, update_sight_lines/3, piece_belongs_to_player/2]).

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
    \+ valid_moves(Board, Player, _),
    format('~w has no valid moves. Game over!~n', [Player]).

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
    stack_for_player(Player, ValidStacks), % Lista de todas as pilhas válidas para o jogador
    member(Row, Board),
    member(Stack, Row),
    member(Stack, ValidStacks), % Verifica se a stack pertence ao jogador
    length(Stack, N),
    N > 1. % Apenas considera pilhas com mais de 1 peça

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

% Prioritize moves: stacks > single pieces; larger stacks > smaller stacks
prioritize_moves(Moves, Board, Player, PrioritizedMoves) :-
    include(stack_move(Board, Player), Moves, StackMoves),
    include(placement_move(Board, Player), Moves, PlacementMoves),
    sort_stacks(StackMoves, SortedStackMoves),
    append(SortedStackMoves, PlacementMoves, PrioritizedMoves).

stack_move(Board, Player, [FromCol, FromRow, ToCol, ToRow]) :-
    position(Board, FromCol, FromRow, Stack),
    stack_belongs_to_player(Stack, Player).

placement_move(Board, Player, [Col, Row]) :-
    position(Board, Col, Row, empty).

sort_stacks(StackMoves, SortedStackMoves) :-
    maplist(stack_size_pair, StackMoves, Pairs),
    keysort(Pairs, SortedPairs),
    pairs_values(SortedPairs, SortedStackMoves).

pairs_values([], []).
pairs_values([_-Value | Rest], [Value | Values]) :-
    pairs_values(Rest, Values).

stack_size_pair(Move, Size-Move) :-
    stack_size(Move, Size).

stack_size([FromCol, FromRow, _, _], Size) :-
    position(Board, FromCol, FromRow, Stack),
    length(Stack, Size).

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
    apply_move(Board, SelectedMove, Player, TempBoard),
    append(MoveHistory, [SelectedMove], NewMoveHistory),
    set_last_played(SelectedMove, Player), % Registra a peça jogada
    NewBoard = TempBoard. % Atualiza o tabuleiro

set_last_played([Col, Row], Player) :-
    retractall(last_played(Player, _)),
    assertz(last_played(Player, [Col, Row])).


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
apply_move(Board, [Col, Row], Player, TempBoard) :-  % Type 1 move (place a piece)
    initial_piece(Player, Piece), % Garante que a peça inicial é usada
    set_position(Board, Col, Row, Piece, TempBoard).


% Define a peça inicial para cada jogador
initial_piece(player1, white_one).
initial_piece(player2, black_one).

apply_move(Board, [FromCol, FromRow, ToCol, ToRow], Player, UpdatedBoard) :-  % Type 2 move (move a stack)
    position(Board, FromCol, FromRow, Stack),
    demote_stack(Stack, NewStack),
    set_position(Board, FromCol, FromRow, NewStack, TempBoard1), % Remove from old position
    stack_for_player(Player, TopPiece),
    set_position(TempBoard1, ToCol, ToRow, TopPiece, TempBoard2), % Place on new position
    update_sight_lines(TempBoard2, Player, UpdatedBoard). % Update sight lines here

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
    findall([Col, Row], 
    (position(Board, Col, Row, Piece), piece_belongs_to_player(Piece, Player)), 
    PlayerPieces),
    last_played(Player, RecentlyPlayed), % Recupera a última peça jogada
    exclude_recently_played(PlayerPieces, RecentlyPlayed, PiecesToPromote),
    promote_pieces(Board, PiecesToPromote, UpdatedBoard).

% Excluir a peça recentemente jogada
exclude_recently_played(PlayerPieces, RecentlyPlayed, PiecesToPromote) :-
    exclude(is_recently_played_or_enemy(RecentlyPlayed), PlayerPieces, PiecesToPromote).

is_recently_played_or_enemy(RecentlyPlayed, [Col, Row]) :-
    RecentlyPlayed = [Col, Row]. % Exclui apenas a peça recentemente jogada

%update stacks and singletons in sight lines
update_stacks_in_sight(Board, [], _, Board).
update_stacks_in_sight(Board, [[Col, Row] | Rest], Player, UpdatedBoard) :-
    find_sight_positions(Board, [Col, Row], SightPositions),
    update_sight_positions(Board, SightPositions, Player, TempBoard),
    update_stacks_in_sight(TempBoard, Rest, Player, UpdatedBoard).

update_sight_positions(Board, [], _, Board).
update_sight_positions(Board, [[Col, Row] | Rest], Player, UpdatedBoard) :-
    position(Board, Col, Row, Piece),
    piece_belongs_to_player(Piece, Player),
    promote_piece(Piece, PromotedPiece),
    set_position(Board, Col, Row, PromotedPiece, TempBoard),
    update_sight_positions(TempBoard, Rest, Player, UpdatedBoard).

update_sight_positions(Board, [_ | Rest], Player, UpdatedBoard) :-
    update_sight_positions(Board, Rest, Player, UpdatedBoard).


%add pieces to all stacks in sight
add_pieces_to_sight(Board, [], _, Board).
add_pieces_to_sight(Board, [[Col, Row] | Rest], Player, NewBoard) :-
    position(Board, Col, Row, Piece),
    (   piece_belongs_to_player(Piece, Player) ->
        promote_piece(Piece, NewPiece),
        set_position(Board, Col, Row, NewPiece, TempBoard);
        TempBoard = Board
    ),
    add_pieces_to_sight(TempBoard, Rest, Player, NewBoard).

%find all positions in sight of a piece (same row, column, and diagonals)
find_sight_positions(Board, [Col, Row], SightPositions) :-
    findall([C, R], (adjacent_space(Board, Col, Row, C, R), position(Board, C, R, _)), SightPositions).

%check if two positions are in line of sight
line_of_sight(Board, [Col1, Row1], [Col2, Row2]) :-
    nonvar(Col1), nonvar(Row1), nonvar(Col2), nonvar(Row2), % Ensure variables are instantiated
    (   Col1 =:= Col2
    ; Row1 =:= Row2
    ; abs(Col1 - Col2) =:= abs(Row1 - Row2)
    ),
    clear_path(Board, [Col1, Row1], [Col2, Row2]).

%check if there are no blocking pieces between two positions
clear_path(Board, [Col1, Row1], [Col2, Row2]) :-
    %calculate steps for direction
    DX is sign(Col2 - Col1),
    DY is sign(Row2 - Row1),
    %Iterate through positions between start and end
    \+ (between(1, max(abs(Col1 - Col2), abs(Row1 - Row2)), Step),
        C is Col1 + Step * DX,
        R is Row1 + Step * DY,
        position(Board, C, R, Piece),
        Piece \= [empty]).

%check if a piece belongs to a player
piece_belongs_to_player(Piece, player1) :-
    member(Piece, [white_one, white_two, white_three, white_four, white_five]).
piece_belongs_to_player(Piece, player2) :-
    member(Piece, [black_one, black_two, black_three, black_four, black_five]).

% Promote pieces in sight lines
promote_pieces(Board, [], Board).
promote_pieces(Board, [[Col, Row] | Rest], NewBoard) :-
    position(Board, Col, Row, Piece),
    promote_piece(Piece, NewPiece),
    set_position(Board, Col, Row, NewPiece, TempBoard),
    promote_pieces(TempBoard, Rest, NewBoard).

demote_stack([Top | Rest], Rest) :- 
    Top \= empty, % Certifica-se de que a stack não está vazia
    !.

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

