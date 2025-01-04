
:- module(game_logic, [game_loop/1, valid_moves/3,game_loop_randplayer/1, game_loop_randbot/1, game_loop_rand/1, apply_move/4, player_has_stack/2, find_placement_moves/2, find_stack_moves/3, promote_pieces/3, update_sight_lines/3, piece_belongs_to_player/2, find_priority_stacks/3, atom_concat/3, atom_number/2, stack_size/2, max_list/2, demote_stack/2, last_played/2, origin_stack/2, list_to_set/2]).

:- use_module(board).
:- use_module(settings).
:- use_module(library(between)).  % Import the 'between' predicate
:- use_module(library(random)).

:- dynamic last_played/2.
:- dynamic origin_stack/2.



last_played(player1, none).
last_played(player2, none).


% Main game loop
game_loop([Board, Player, MoveHistory, TurnCount]) :-
    display_game(Board),
    (   game_over(Board, Player) ->
        declare_winner(Player);
        valid_moves(Board, Player, Moves),
        play_turn(Board, Player, Moves, MoveHistory, TurnCount, NewBoard, NewMoveHistory),
        update_sight_lines(NewBoard, Player, UpdatedBoard), % Update sight lines here
        retractall(origin_stack(Player, _)),  % Remove origem usada
        next_player(Player, NextPlayer),
        NewTurnCount is TurnCount + 1,
        game_loop([UpdatedBoard, NextPlayer, NewMoveHistory, NewTurnCount])
    ).

game_loop_randplayer([Board, Player, MoveHistory, TurnCount]) :-
    display_game(Board),
    (   game_over(Board, Player) ->
        declare_winner(Player);
        valid_moves(Board, Player, Moves),
        play_turn(Board, Player, Moves, MoveHistory, TurnCount, NewBoard, NewMoveHistory),
        update_sight_lines(NewBoard, Player, UpdatedBoard), % Update sight lines here
        next_player(Player, NextPlayer),
        NewTurnCount is TurnCount + 1,
        game_loop_randbot([UpdatedBoard, NextPlayer, NewMoveHistory, NewTurnCount])
    ).
game_loop_randbot([Board, Player, MoveHistory, TurnCount]) :-
    (   game_over(Board, Player) ->
        declare_winner(Player);
        valid_moves(Board, Player, Moves),
        play_turn_random(Board, Player, Moves, MoveHistory, TurnCount, NewBoard, NewMoveHistory),
        update_sight_lines(NewBoard, Player, UpdatedBoard), % Update sight lines here
        next_player(Player, NextPlayer),
        NewTurnCount is TurnCount + 1,
        game_loop_randplayer([UpdatedBoard, NextPlayer, NewMoveHistory, NewTurnCount])
    ).
game_loop_smartplayer([Board, Player, MoveHistory, TurnCount]) :-
    display_game(Board),
    (   game_over(Board, Player) ->
        declare_winner(Player);
        valid_moves(Board, Player, Moves),
        play_turn(Board, Player, Moves, MoveHistory, TurnCount, NewBoard, NewMoveHistory),
        update_sight_lines(NewBoard, Player, UpdatedBoard), % Update sight lines here
        next_player(Player, NextPlayer),
        NewTurnCount is TurnCount + 1,
        game_loop_smartbot([UpdatedBoard, NextPlayer, NewMoveHistory, NewTurnCount])
    ).
game_loop_smartbot([Board, Player, MoveHistory, TurnCount]) :-
    display_game(Board),
    (   game_over(Board, Player) ->
        declare_winner(Player);
        valid_moves(Board, Player, Moves),
        play_turn_smart(Board, Player, Moves, MoveHistory, TurnCount, NewBoard, NewMoveHistory),
        update_sight_lines(NewBoard, Player, UpdatedBoard), % Update sight lines here
        next_player(Player, NextPlayer),
        NewTurnCount is TurnCount + 1,
        game_loop_smartplayer([UpdatedBoard, NextPlayer, NewMoveHistory, NewTurnCount])
    ).
game_loop_rand([Board, Player, MoveHistory, TurnCount]) :-
    display_game(Board),
    (   game_over(Board, Player) ->
        declare_winner(Player);
        valid_moves(Board, Player, Moves),
        play_turn_random(Board, Player, Moves, MoveHistory, TurnCount, NewBoard, NewMoveHistory),
        update_sight_lines(NewBoard, Player, UpdatedBoard), % Update sight lines here
        next_player(Player, NextPlayer),
        NewTurnCount is TurnCount + 1,
        game_loop_rand([UpdatedBoard, NextPlayer, NewMoveHistory, NewTurnCount])
    ).
game_loop_smart([Board, Player, MoveHistory, TurnCount]) :-
    display_game(Board),
    (   game_over(Board, Player) ->
        declare_winner(Player);
        valid_moves(Board, Player, Moves),
        play_turn_smart(Board, Player, Moves, MoveHistory, TurnCount, NewBoard, NewMoveHistory),
        update_sight_lines(NewBoard, Player, UpdatedBoard), % Update sight lines here
        next_player(Player, NextPlayer),
        NewTurnCount is TurnCount + 1,
        game_loop_smart([UpdatedBoard, NextPlayer, NewMoveHistory, NewTurnCount])
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

% Encontra as stacks prioritárias (maior altura) de um jogador no tabuleiro
find_priority_stacks(Board, Player, PriorityStacks) :-
    % Obter todas as stacks do jogador com suas alturas
    findall([Col, Row, Size],
        (position(Board, Col, Row, Stack),
         piece_belongs_to_player(Stack, Player),
         stack_size(Stack, Size)), 
        PlayerStacks),
    % Encontra a altura máxima entre todas as stacks
    findall(Size, member([_, _, Size], PlayerStacks), Sizes),
    max_list(Sizes, MaxSize),  % Determina a altura máxima
    % Filtra stacks com a altura máxima
    include(has_max_size(MaxSize), PlayerStacks, PriorityStacks).

% Verifica se a stack tem a altura máxima
has_max_size(MaxSize, [_, _, Size]) :-
    Size =:= MaxSize.

% Base: Se a lista tem apenas um elemento, este é o máximo.
max_list([X], X).

% Recursão: O máximo é o maior entre a cabeça e o máximo do restante da lista.
max_list([H | T], Max) :-
    max_list(T, TailMax),
    Max is max(H, TailMax).

find_stack_moves(Board, Player, Moves) :-
    find_priority_stacks(Board, Player, PriorityStacks),  % Obtemos as stacks prioritárias
    findall([FromCol, FromRow, ToCol, ToRow],
        (   member([FromCol, FromRow, _], PriorityStacks),  % Para cada stack prioritária
            adjacent_space(Board, FromCol, FromRow, ToCol, ToRow),
            position(Board, ToCol, ToRow, empty)  % Certifique-se de que o destino está vazio
        ),
        Moves).
        
% Check if a player has a stack on the board (only consider stacks with more than 1 piece)
player_has_stack(Board, Player) :-
    player_stacks(Board, Player, Stacks),
    Stacks \= []. % Verifica se há ao menos uma stack

player_stacks(Board, Player, Stacks) :-
    findall([Col, Row, Size],
        (position(Board, Col, Row, Stack), 
         piece_belongs_to_player(Stack, Player),
         stack_size(Stack, Size), 
         Size > 1), % Apenas considera stacks
        Stacks).

% Determinar a altura de uma stack
stack_size(Stack, Size) :-
    sub_atom(Stack, _, 1, 0, DigitChar),  % Extrai o último caractere do átomo
    char_code(DigitChar, DigitCode),
    DigitCode >= 49, % Código ASCII para '1'
    DigitCode =< 57, % Código ASCII para '9'
    Size is DigitCode - 48. % Converte código ASCII para número

sub_atom(Atom, Start, Length, After, SubAtom) :-
    atom_chars(Atom, CharList), % Converte o átomo em uma lista de caracteres
    length(CharList, TotalLength),
    StartEnd is Start + Length,
    StartEnd =< TotalLength,
    length(Prefix, Start), % Pega os primeiros 'Start' caracteres
    append(Prefix, Rest, CharList), % Divide em prefixo e resto
    length(Mid, Length), % Define o comprimento da substring
    append(Mid, Suffix, Rest), % Divide o resto em substring e sufixo
    length(Suffix, After), % Verifica o comprimento do sufixo
    atom_chars(SubAtom, Mid). % Converte a substring de volta em átomo

char_code(Char, Code) :-
    atom_chars(Char, [C]), % Converte o átomo em uma lista de caracteres (deve ser único)
    !, % Garante que é apenas um caractere
    char_code_list(C, Code). % Procura o código na tabela

% Tabela de mapeamento manual
char_code_list('A', 65).
char_code_list('B', 66).
char_code_list('C', 67).

char_code_list('0', 48).
char_code_list('1', 49).
char_code_list('2', 50).
char_code_list('3', 51).
char_code_list('4', 52).
char_code_list('5', 53).


% Find all valid type 1 (placement) moves
find_placement_moves(Board, Moves) :-
    findall([Col, Row], position(Board, Col, Row, empty), Moves).


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
    format('Starting turn for ~w~n', [Player]),
    format('Available moves: ~w~n', [Moves]),
    prompt_move(Player, Moves, SelectedMove),
    format('Selected move: ~w~n', [SelectedMove]),
    (   apply_move(Board, SelectedMove, Player, TempBoard) ->
        append(MoveHistory, [SelectedMove], NewMoveHistory),
        NewBoard = TempBoard,
        format('Turn completed for ~w~n', [Player])
    ;   write('Move failed, prompting again.'), nl,
        fail  % Retorna ao `repeat` em `prompt_move`
    ).

play_turn_random(Board, Player, Moves, MoveHistory, TurnCount, NewBoard, NewMoveHistory) :-
    format('~w, it is your turn.~n', [Player]),
    format('Valid moves: ~w~n', [Moves]),
    random_move(Moves, SelectedMove),  % Select a random move
    format('Selected move: ~w~n', [SelectedMove]),  % Debugging line
    apply_move(Board, SelectedMove, Player, TempBoard),
    append(MoveHistory, [SelectedMove], NewMoveHistory),
    set_last_played(SelectedMove, Player), % Registra a peça jogada
    NewBoard = TempBoard. % Atualiza o tabuleiro

random_move(Moves, SelectedMove) :-
    random_member(SelectedMove, Moves).

play_turn_smart(Board, Player, Moves, MoveHistory, TurnCount, NewBoard, NewMoveHistory) :-
    format('~w, it is your turn.~n', [Player]),
    format('Valid moves: ~w~n', [Moves]),
    find_best_move(Board, Player, Moves, BestMove),
    format('Selected move: ~w~n', [BestMove]),
    apply_move(Board, BestMove, Player, TempBoard),
    append(MoveHistory, [BestMove], NewMoveHistory),
    set_last_played(BestMove, Player),
    NewBoard = TempBoard.

% Find the best move by scoring all valid moves
find_best_move(Board, Player, Moves, BestMove) :-
    maplist(score_move(Board, Player), Moves, ScoredMoves),
    max_member(_-BestMove, ScoredMoves).

% Score a move based on strategic considerations
score_move(Board, Player, Move, Score-Move) :-
    evaluate_move(Board, Player, Move, Score).

% Evaluate the score of a move
evaluate_move(Board, Player, Move, Score) :-
    (   leads_to_promotion(Board, Player, Move) -> PromotionScore = 10; PromotionScore = 0),
    (   threatens_opponent(Board, Player, Move) -> ThreatScore = 5; ThreatScore = 0),
    (   defends_priority_stack(Board, Player, Move) -> DefenseScore = 7; DefenseScore = 0),
    (   central_position(Board, Move) -> CentralScore = 3; CentralScore = 0),
    Score is PromotionScore + ThreatScore + DefenseScore + CentralScore.

% Example helper functions for scoring criteria
leads_to_promotion(Board, Player, [Col, Row]) :-
    position(Board, Col, Row, Piece),
    promote_piece(Piece, _).

threatens_opponent(Board, Player, [ToCol, ToRow]) :-
    next_player(Player, Opponent),
    position(Board, ToCol, ToRow, Stack),
    piece_belongs_to_player(Stack, Opponent).

defends_priority_stack(Board, Player, [FromCol, FromRow, ToCol, ToRow]) :-
    position(Board, FromCol, FromRow, Stack),
    stack_size(Stack, Size),
    Size > 1,
    piece_belongs_to_player(Stack, Player).

central_position(Board, [Col, Row]) :-
    board_size(Board, Size),
    Middle is Size // 2,
    abs(Col - Middle) =< 1,
    abs(Row - Middle) =< 1.


set_last_played([Col, Row], Player) :-
    retractall(last_played(Player, _)),
    assertz(last_played(Player, [Col, Row])).


% Prompt the player to choose a move
prompt_move(Player, Moves, SelectedMove) :-
    repeat,
    format('Prompting move for ~w~n', [Player]), % Debugging line
    write('Enter your move (format: [Col, Row] or [FromCol, FromRow, ToCol, ToRow]): '),
    read(SelectedMove),
    format('You entered: ~w~n', [SelectedMove]), % Debugging line
    (   member(SelectedMove, Moves) ->
        format('Valid move: ~w~n', [SelectedMove]),  % Adicione esta linha
        true;
        write('Invalid move, try again.'), nl, fail
    ).

% Apply the selected move to the board
apply_move(Board, [Col, Row], Player, TempBoard) :-  % Type 1 move (place a piece)
    initial_piece(Player, Piece), % Garante que a peça inicial é usada
    set_position(Board, Col, Row, Piece, TempBoard),
    set_last_played([Col, Row], Player).


% Define a peça inicial para cada jogador
initial_piece(player1, white1).
initial_piece(player2, black1).

apply_move(Board, [FromCol, FromRow, ToCol, ToRow], Player, UpdatedBoard) :-
    format('Applying stack move: [~w,~w] -> [~w,~w] for ~w~n', [FromCol, FromRow, ToCol, ToRow, Player]),
    position(Board, FromCol, FromRow, Stack),
    stack_size(Stack, Size),
    (   Size > 1 ->  % Apenas stacks com mais de 1 peça podem ser movidas
        demote_stack(Stack, NewStack),
        set_position(Board, FromCol, FromRow, NewStack, TempBoard1),  % Atualiza a origem
        initial_piece(Player, TopPiece),
        set_position(TempBoard1, ToCol, ToRow, TopPiece, TempBoard2), % Atualiza o destino
        retractall(origin_stack(Player, _)),  % Remove origem anterior
        assertz(origin_stack(Player, [FromCol, FromRow])),  % Registra a origem atual
        set_last_played([ToCol, ToRow], Player),  % Registra o destino como a última jogada
        UpdatedBoard = TempBoard2,
        format('Move applied successfully.~n', [])
    ;   write('Move failed: Stack size is not valid or demotion failed.'), nl, fail
    ).

% Get the stack corresponding to the player (this ensures the correct symbols are used)
stack_for_player(player1, Stack) :-
    member(Stack, [white1, white2, white3, white4, white5]).
stack_for_player(player2, Stack) :-
    member(Stack, [black1, black2, black3, black4, black5]).

% Determine the next player
next_player(player1, player2).
next_player(player2, player1).

% Update the sight lines after a move: Check up, down, left, right, and diagonals
update_sight_lines(Board, Player, UpdatedBoard) :-
    last_played(Player, RecentlyPlayed), % Recupera a última peça jogada
    % Recupera origem (se for uma jogada de stack)
    (   origin_stack(Player, Origin) -> true; Origin = none),
    % Encontra peças para promoção
    findall([Col, Row], 
        (position(Board, Col, Row, Piece), 
         piece_belongs_to_player(Piece, Player),
         [Col, Row] \= RecentlyPlayed,  % Exclui peça recém-jogada
         (Origin = none; [Col, Row] \= Origin)  % Exclui origem, se existir
        ), 
        PlayerPieces),
    list_to_set(PlayerPieces, UniquePlayerPieces),
    promote_pieces(Board, UniquePlayerPieces, UpdatedBoard).


list_to_set([], []).
list_to_set([H | T], Set) :-
    member(H, T), !,
    list_to_set(T, Set).
list_to_set([H | T], [H | Set]) :-
    list_to_set(T, Set).


% Excluir a peça recentemente jogada
exclude_recently_played(PlayerPieces, RecentlyPlayed, PiecesToPromote) :-
    findall(Piece,
                (   member(Piece, PlayerPieces),
                    \+ is_recently_played_or_enemy(RecentlyPlayed, Piece)
                ),
                PiecesToPromote).
is_recently_played_or_enemy(RecentlyPlayed, [Col, Row]) :-
    RecentlyPlayed = [Col, Row]. % Exclui apenas a peça recentemente jogada


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
    member(Piece, [white1, white2, white3, white4, white5]).
piece_belongs_to_player(Piece, player2) :-
    member(Piece, [black1, black2, black3, black4, black5]).

% Promote pieces in sight lines
promote_pieces(Board, [], Board).
promote_pieces(Board, [[Col, Row] | Rest], NewBoard) :-
    position(Board, Col, Row, Piece),
    promote_piece(Piece, NewPiece),
    set_position(Board, Col, Row, NewPiece, TempBoard),
    promote_pieces(TempBoard, Rest, NewBoard).

% Demote a piece to the next lower level
demote_stack(Stack, DemotedStack) :-
    promote_piece(DemotedStack, Stack), % Reutiliza a lógica de promoção invertida
    Stack \= DemotedStack, % Garante que houve "demotion"
    !. % Caso contrário, falha

% Promote a piece (e.g., white_one -> white_two)
promote_piece(white1, white2).
promote_piece(white2, white3).
promote_piece(white3, white4).
promote_piece(white4, white5).
promote_piece(black1, black2).
promote_piece(black2, black3).
promote_piece(black3, black4).
promote_piece(black4, black5).
promote_piece(Piece, Piece).  % No promotion if it's already at the highest level
