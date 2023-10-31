:- consult('data.pl').

% Define a predicate to display the board
display_board(Board) :-
    nl,
    display_separator,
    display_rows(Board),
    nl,
    display_current_player,
    display_remaining_pieces,
    display_counter_position.

display_board(Board, Row1, Row2, Col1, Col2) :-
    nl,
    display_separator,
    display_rows(Row1, Row2, Col1, Col2, 1, Board),
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

display_rows([]).
display_rows([Row|Rows]) :-
    display_row(Row),
    display_separator,
    display_rows(Rows).
display_row([]) :- nl.
display_row([Cell|Rest]) :-
    write(' | '), write(Cell),
    display_row(Rest).

display_rows(_, _, _, _, _, []).
display_rows(Row1, Row2, Col1, Col2, CurrentRow, [Row|Rows]) :-
    display_row(Row1, Row2, Col1, Col2, CurrentRow, 1, Row, 0),
    display_separator,
    NextRow is CurrentRow + 1,
    display_rows(Row1, Row2, Col1, Col2, NextRow, Rows).

display_row(_, _, _, _, _, _, [], _) :- nl.
display_row(Row1, Row2, Col1, Col2, CurrentRow, CurrentCol, [Cell|Rest], HasWritten) :-
    (Row1 =< CurrentRow, CurrentRow =< Row2, Col1 =< CurrentCol, CurrentCol =< Col2, HasWritten == 0 -> write(' | '), write(Cell), HasWritten1 is 1
    ;
    Row1 =< CurrentRow, CurrentRow =< Row2, Col1 =< CurrentCol, CurrentCol =< Col2 -> write('   '), write(Cell), HasWritten1 is HasWritten
    ;
    write(' | '), write(Cell), HasWritten1 is HasWritten),
    NextCol is CurrentCol + 1,
    display_row(Row1, Row2, Col1, Col2, CurrentRow, NextCol, Rest, HasWritten1).


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
start_board :-
    board(_, Board),
    display_board(Board).