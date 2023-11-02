:- consult('data.pl').

% Define a predicate to display the board
display_board(Board) :-
    nl,
    display_separator,
    display_rows(1,Board),
    nl,
    display_current_player,
    display_remaining_pieces,
    display_counter_position.

% Two clear lines
blank_lines :-
    nl, nl.

% Clear screen
clear :-
    write('\e[2J').

display_rows(_, []) :- nl.
display_rows(CurrentRow, [Row|Rows]) :-
    display_row(CurrentRow, 1, Row, 0),
    display_separator,
    NextRow is CurrentRow + 1,
    display_rows(NextRow, Rows).

display_row(_, _, [], _) :- nl.
display_row(CurrentRow, CurrentCol, [Cell|Rest], HasWritten) :-
    findall(LastMove, last_move(LastMove), LastMoves),
    (LastMoves = [] ->
        write(' | '), write(Cell), HasWritten1 is HasWritten
    ;
        % Check if the current cell is in the range specified by LastMoves
        (member(Row1-Col1-Col2-Row2, LastMoves), Row1 =< CurrentRow, CurrentRow =< Row2, Col1 =< CurrentCol, CurrentCol =< Col2 ->
            (HasWritten == 0 ->
                write(' | '), write(Cell), HasWritten1 is 1
            ;
                write('   '), write(Cell), HasWritten1 is HasWritten)
        ;
            write(' | '), write(Cell), HasWritten1 is HasWritten)
    ),

    NextCol is CurrentCol + 1,
    display_row(CurrentRow, NextCol, Rest, HasWritten1).


display_separator :-
    write(' |----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----| '), nl.
    

% Predicate to display current player
display_current_player :-
    current_player(Player),
    format('Current player: ~w~n', [Player]),
    nl.


% Define a predicate to display the counter position
display_counter_position :-
    findall(score_counter(Player, Row, Col), score_counter(Player, Row, Col), CounterPositions),
    display_counter_position(CounterPositions).

display_counter_position([]).
display_counter_position([score_counter(Player, Row, Col)|Rest]) :-
    Score is Row*10 + Col,
    format('Counter position for ~w player: (~w, ~w)~n', [Player, Row, Col]),
    format('Score for ~w player: ~w~n~n', [Player, Score]),
    display_counter_position(Rest).

% Define a predicate to display the winner
display_winner :-
    write('No more tiles to remove. Game ends. \nPlayer Dark wins!').

% Define a predicate to the start board
start_board(Board) :-
    board(_, Board),
    display_board(Board).

% Define a predicate to start the game screen
game_start :-
    write('PLACEMENT PHASE: \n'),
    start_board(Board),
    validate_piece.
