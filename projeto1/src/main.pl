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

% play/0
% initiate the game
play :-
    clear_data,
    configurations(GameState), !,
    game_cycle(GameState).