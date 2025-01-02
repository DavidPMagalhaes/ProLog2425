
:- module(game_logic, [game_loop/1, game_loop_randplayer/1, game_loop_randbot/1, game_loop_rand/1, valid_moves/3, apply_move/4, player_has_stack/2, find_placement_moves/2, find_stack_moves/3, promote_pieces/3, update_sight_lines/3, piece_belongs_to_player/2, find_priority_stacks/3, atom_concat/3, atom_number/2, stack_size/2, max_list/2, demote_stack/2]).

:- use_module(board).
:- use_module(settings).
:- use_module(library(between)).  % Import the 'between' predicate
:- use_module(library(random)).

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
    findall([Col, Row, Size],
        (member([Col, Row, Size], PlayerStacks), Size =:= MaxSize),
        PriorityStacks).


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
    format('~w, it is your turn.~n', [Player]),
    format('Valid moves: ~w~n', [Moves]),
    prompt_move(Player, Moves, SelectedMove),
    apply_move(Board, SelectedMove, Player, TempBoard),
    append(MoveHistory, [SelectedMove], NewMoveHistory),
    set_last_played(SelectedMove, Player), % Registra a peça jogada
    NewBoard = TempBoard. % Atualiza o tabuleiro

% Play a turn: randomly select a move and update the game state
play_turn_random(Board, Player, Moves, MoveHistory, TurnCount, NewBoard, NewMoveHistory) :-
    format('~w, it is your turn.~n', [Player]),
    format('Valid moves: ~w~n', [Moves]),
    random_move(Moves, SelectedMove),  % Select a random move
    format('Selected move: ~w~n', [SelectedMove]),  % Debugging line
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
initial_piece(player1, white1).
initial_piece(player2, black1).

apply_move(Board, [FromCol, FromRow, ToCol, ToRow], Player, UpdatedBoard) :-
    % Atualizar a posição inicial para "empty"
    position(Board, FromCol, FromRow, Stack),
    stack_size(Stack, Size),
    (   Size > 1
    ->  demote_stack(Stack, NewStack),
        set_position(Board, FromCol, FromRow, NewStack, TempBoard1)
    ;   set_position(Board, FromCol, FromRow, empty, TempBoard1)
    ),
    % Colocar a nova peça na posição final
    apply_move(TempBoard1, [ToCol, ToRow], Player, UpdatedBoard).

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
    findall([Col, Row], 
    (position(Board, Col, Row, Piece), piece_belongs_to_player(Piece, Player)), 
    PlayerPieces),
    last_played(Player, RecentlyPlayed), % Recupera a última peça jogada
    exclude_recently_played(PlayerPieces, RecentlyPlayed, PiecesToPromote),
    promote_pieces(Board, PiecesToPromote, UpdatedBoard).

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

random_move(Moves, SelectedMove) :-
    random_member(SelectedMove, Moves).