:- consult('data.pl').

% display_board(+GameState)
display_board(GameState) :-
    [Board, Player, _] = GameState,
    nl,
    display_separator,
    display_rows(1,Board),
    nl,
    display_current_player(Player),
    display_remaining_pieces(Player),
    display_counter_position,
    display_scores.

% Two clear lines
blank_lines :-
    nl, nl.

% Clear screen
clear :-
    write('\e[2J').

% display_rows(+Row, +ListofRows)
display_rows(_, []) :- nl.
display_rows(CurrentRow, [Row|Rows]) :-
    display_row(CurrentRow, 1, Row, 0),
    display_separator(CurrentRow),
    NextRow is CurrentRow + 1,
    display_rows(NextRow, Rows).

% display_row(+CurrentRow, +CurrentCol, +ListofCells, -LastMoveEnd)
display_row(_, _, [], _) :- nl.
display_row(CurrentRow, CurrentCol, [Cell|Rest], LastMoveEnd) :-
    findall(LastMove, last_move(LastMove), LastMoves),
    (LastMoves = [] ->
        write(' | '), write(Cell), NewLastMoveEnd = 0
    ;
        (member(Row1-Col1-Col2-Row2-_-_, LastMoves), Row1 == CurrentRow, CurrentRow == Row2, Col1 =< CurrentCol, CurrentCol =< Col2 ->
            (CurrentCol > LastMoveEnd -> write(' | '), write(Cell), NewLastMoveEnd = Col2 ; write('   '), write(Cell), NewLastMoveEnd = LastMoveEnd)
        ;   
            write(' | '), write(Cell), NewLastMoveEnd = 0
        )
    ),
    NextCol is CurrentCol + 1,
    display_row(CurrentRow, NextCol, Rest, NewLastMoveEnd).

% it displays the sperators of the board
display_separator :-
    write(' |----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----| '), nl.

% display_separator(+CurrentRow)
display_separator(CurrentRow) :-
    findall(LastMove, last_move(LastMove), LastMoves),
    display_separator_columns(1, CurrentRow, LastMoves).

% display_separator_columns(+CurrentCol, +CurrentRow, -LastMoves)
display_separator_columns(13, _, _) :- write('|'), nl.
display_separator_columns(CurrentCol, CurrentRow, LastMoves) :-
    (member(Row1-Col1-Col2-Row2-_-_, LastMoves), Row1 =< CurrentRow, CurrentRow < Row2, Col1 == CurrentCol, CurrentCol == Col2, Row1 \= Row2 ->
        write('|     ')
    ;
        (CurrentCol == 1 ->
            write(' |----')
        ;
            write('|-----')
        )
    ),
    NextCol is CurrentCol + 1,
    display_separator_columns(NextCol, CurrentRow, LastMoves).

% Predicate to display current player
% display_current_player(+Player)
display_current_player(Player) :-
    format('Current player: ~w~n', [Player]),
    nl.

% Define a predicate to display the counter position
% display_counter_position(+CounterPos)
display_counter_position :-
    findall(score_counter(Player,Col, Row), score_counter(Player,Col, Row), CounterPositions),
    display_counter_position(CounterPositions).

display_counter_position([]) :- nl.
display_counter_position([score_counter(Player,Col, Row)|Rest]) :-
    format('Counter position for ~w player: (~w, ~w)~n', [Player,Col, Row]),
    display_counter_position(Rest).


% Define a predicate to display the scores
display_scores :-
    player_score('Light', LightScore),
    player_score('Dark', DarkScore),
    format('Light player score: ~w~n', [LightScore]),
    format('Dark player score: ~w~n', [DarkScore]),
    nl.


% Define a predicate to display the winner
display_winner :-
    write('No more tiles to remove. Game ends. \nPlayer Dark wins!').