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
start_board :-
    board(_, Board),
    display_board(Board).

% Define a predicate to start the game screen
game_start :-
    write('PLACEMENT PHASE: \n'),
    start_board,
    validate_piece.


% Define a predicate to validate the user's piece selection
validate_piece :-
    get_move(Col1-Row1-Col2-Row2, PieceOption),
    pieceoption(PieceOption),
    place_piece(PieceOption, Row1, Col1, Col2, Row2).

% Define a predicate to select a piece option with active selection
pieceoption(PieceOption) :-
    between(1, 5, PieceOption),
    current_player(Player),
    (PieceOption =:= 5 -> Value is 6 ; Value is PieceOption), % Use the if-then-else construct to conditionally set the value of Value
    player_value_pieces(Player, _, _, Value), !. % Use Value instead of PieceOption

place_piece(PieceOption, Row1, Col1, Col2, Row2) :-
    clear,
    current_player(Player),
    (PieceOption =:= 5 -> Value is 6 ; Value is PieceOption),
    player_value_pieces(Player, Count, Size, Value),
    NewCount is Count - 1,
    retract(player_value_pieces(Player, Count, Size, Value)),
    assert(player_value_pieces(Player, NewCount, Size, Value)),
    board(BoardId, OldBoard),
    Temprow is Row1 + 1,
    Temprow2 is Row2 + 1,
    Tempcol is Col1 + 1,
    Tempcol2 is Col2 + 1,
    replace(OldBoard, Temprow, Tempcol2, Temprow2, Tempcol, Value, NewBoard),
    retract(board(BoardId, OldBoard)),
    assert(board(BoardId, NewBoard)),
    display_board(NewBoard).

% Define a predicate to replace the value on the board at the specified row and column
replace(OldBoard, Row1, Col2, Row2, Col1, Value, NewBoard) :-
    format('Row1: ~w, Col1: ~w, Row2: ~w, Col2: ~w, Value: ~w~n', [Row1, Col1, Row2, Col2, Value]),
    ( Row1 == Row2 ->
        nth1(Row1, OldBoard, OldRow),
        current_player(Player),
        replace_row(OldRow, Col1, Col2, Value, Player, NewRow),
        replace_list(OldBoard, Row1, NewRow, NewBoard)
     ;Col1 == Col2 ->
        transpose(OldBoard, TransposedBoard),
        nth1(Row1, TransposedBoard, OldRow),
        current_player(Player),
        replace_row(OldRow, Col1, Col2, Value, Player, NewRow),
        replace_list(TransposedBoard, Row1, NewRow, TempBoard),
        transpose(TempBoard, NewBoard)
    ).

% Define a predicate to replace the value at the specified index in a list
replace_list([_|T], 1, X, [X|T]).
replace_list([H|T], I, X, [H|R]) :-
    I > 1,
    NI is I - 1,
    replace_list(T, NI, X, R).

% Define a predicate to replace a range of values at the specified indices in a row
replace_row(Row, Col1, Col2, X, Player, NewRow) :-
    length(Row, Length),
    number_chars(X, XChars),
    atom_chars(XAtom, XChars),
    sub_atom(Player, 0, 1, _, PlayerInitial),  % Extract the first character of the Player atom
    sort([Col1, Col2], [SortedCol1, SortedCol2]),
    findall(Y, (between(1, Length, I), (I >= Col1, I =< Col2 -> atom_concat(PlayerInitial, '-', Temp), atom_concat(Temp, XAtom, Y); nth1(I, Row, Y))), NewRow).