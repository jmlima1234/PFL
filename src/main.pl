:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(clpfd)).
:- consult('display.pl').
:- consult('Menu_configurations.pl').
:- consult('utils.pl').
:- consult('placementphase.pl').
:- consult('scoringphase.pl').

% in_bounds(+Board,+Coordinate)
% Checks if calculated coordinate is inside Board
check_bounds(Board, Col-Row) :-
    length(Board, Size),
    between(1, Size, Col),
    between(1, Size, Row).

% validate_move(+Board, +CoordsOrigin, +CoordsDestination)
% Checks if the move is valid or not on the placement phase
validate_move_PP(_, _, _, -1) :-
    write('Valid Move!').
validate_move_PP(GameState, ColI-RowI, ColF-RowF, Size) :-
    [Board, Player, _] = GameState,
    \+passed(Player),
    check_bounds(Board, ColI-RowI),
    check_bounds(Board, ColF-RowF),
    nth1(RowI, Board, RowList),
    nth1(ColI, RowList, Cell),
    (Cell == ' - ' ->
        (
            TempVarCol is ColF - ColI + 1,
            TempVarRow is RowF - RowI + 1,
            (TempVarCol == Size ->
                Col is ColI + 1,
                NewSize is Size - 1,
                (NewSize > 0 -> validate_move_PP(GameState, Col-RowI, ColF-RowF, NewSize) ; write('Valid Move!'))
            ; TempVarRow == Size ->
                Row is RowI + 1,
                NewSize is Size - 1,
                (NewSize > 0 -> validate_move_PP(GameState, ColI-Row, ColF-RowF, NewSize) ; write('Valid Move!'))
            ; 
                (difficulty(Player, Level) -> 
                    fail
                ;
                    write('Invalid Move! Size is incorrect\n'),
                    fail
                )
            )
        )
    ; % Cell is not empt
        (difficulty(Player, Level) -> 
            fail
        ;
            write('Invalid Move! Cell is not empty\n'),
            fail
        )
    ).


% Check if there are any possible moves for the current player
has_possible_moves(GameState, Moves) :-
    [Board, Player, _] = GameState,
    player_value_pieces(Player, Pieces, _,  Size, _),
    has_possible_moves(Board, Player, Pieces, Moves, Size).

% Base case: No pieces left, no possible moves
has_possible_moves(_, _, 0, _, _) :- fail.

% If there are pieces left, check for possible moves
has_possible_moves(Board, Player, Pieces, Moves, Size) :-
    check_possible_moves(Board, Player, Pieces, 1, 1, Moves, Size).

% Check for possible moves starting from a specific row and column
check_possible_moves(_, _, _, Row, _, _, _) :- Row > 10, !, fail.

check_possible_moves(Board, Player, Pieces, Row, 10, Moves, Size) :-
    NewRow is Row + 1,
    check_possible_moves(Board, Player, Pieces, NewRow, 1, Moves, Size).

check_possible_moves(_, _, _, 10, 10, _, _) :-
    write('No more valid moves, the player should pass! \n').

check_possible_moves(Board, Player, Pieces, Row, Col, Moves, Size) :-
    Col < 11,
    ColF is Col + Size,
    RowF is Row + Size,
    (
        (validate_move_PP([Board, Player, _], Col-Row, ColF-Row, Size) ->
            append(Moves, [Col-Row, ColF-Row], NewMoves)
        ; validate_move_PP([Board, Player, _], Col-Row, Col-RowF, Size) ->
            append(Moves, [Col-Row, Col-RowF], NewMoves)
        ; true
        )
    ),
    NewPieces is Pieces - 1,
    NewPieces > 0,
    % Continue checking the next cell and the next piece
    NextCol is Col + 1,
    check_possible_moves(Board, Pieces, Row, NextCol).

% game_cycle(+GameState)
% Loop that keeps the game running
game_cycle(GameState):-
    [_, _, Phase] = GameState,
    Phase == 'game_over', 
    game_over(GameState, Winner),
    format('No more tiles to remove. Game Over.~nPlayer ~w wins!', [Winner]), !,
    reset_game.
game_cycle(GameState):-
    [Board, _, Phase] = GameState,
    Phase == 'Placement Phase', 
    display_board(GameState),
    validate_piece(GameState, Board, NewGameState), !,  
    game_cycle(NewGameState).
game_cycle(GameState):-
    [_, _, Phase] = GameState,
    Phase == 'Scoring Phase',
    display_board(GameState),
    scoringphase_start(GameState, NewGameState), !,
    game_cycle(NewGameState).

play :-
    clear_data,
    configurations(GameState), !,
    game_cycle(GameState).