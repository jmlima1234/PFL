:- consult('data.pl').
:- consult('display.pl').
:- consult('placementphase.pl').


check_all_moves([], _, _) :-
    write('false'), !.

check_all_moves([Row1-Col1-Col2-Row2-Player-Value|Rest], Row, Col) :-
    % Check if the last move occupies the given row and column
    (occupies(Row1-Col1-Col2-Row2, Row, Col) ->
        pieces_same_line(Row1-Col1-Col2-Row2, Count),
        format('Count: ~w~n', [Count]),
        counter_same_line(Row1-Col1-Col2-Row2, Counter),
        format('Counter: ~w~n', [Counter]),
        max_points(Player-Value, Count, Counter, MaxPoints),
        format('MaxPoints: ~w~n', [MaxPoints]),
        retract(last_move(Row1-Col1-Col2-Row2-Player-Value)),  % Remove the piece from last_move/1 if it's found
        remove_piece(Row1-Col1-Col2-Row2), !
    ;
        check_all_moves(Rest, Row, Col)).  % Continue with the rest of the list

% Helper function to check if a last move occupies a given row and column
occupies(Row1-Col1-Col2-Row2, Row, Col) :-
    between(Row1, Row2, Row),
    between(Col1, Col2, Col).

remove_piece(Row, Col) :-
    % Iterate through the last moves
    findall(LastMove, last_move(LastMove), LastMoves),
    TempRow is Row + 1,
    TempCol is Col + 1,
    check_all_moves(LastMoves, TempRow, TempCol).

remove_piece(Row1-Col1-Col2-Row2) :-
    %clear,
    board(BoardId, OldBoard),
    empty_cell(OldBoard, Row1, Col1, Row2, Col2, NewBoard),
    retract(board(BoardId, OldBoard)),
    assert(board(BoardId, NewBoard)),
    display_board(NewBoard).

pieces_same_line(Row1-Col1-Col2-Row2, Count) :-
    (Row1 == Row2 -> 
        findall(_, (last_move(Row-_-_-_-_-_), Row == Row1), List),
        length(List, TempCount),
        Count is TempCount - 1
    ;Col1 == Col2 ->
        findall(_, (last_move(_-Col-_-_-_-_), Col == Col1), List),
        length(List, TempCount),
        Count is TempCount - 1
    ).

counter_same_line(Row1-Col1-Col2-Row2, Counter) :-
    (Row1 == Row2 -> 
        findall(_, (score_counter(_,Row,_), 
        TempRow is 11 - Row, TempRow == Row1), List),
        length(List, Counter)
    ;Col1 == Col2 ->
        findall(_, (score_counter(_,_,Col),
        TempCol is Col + 2, TempCol == Col1), List),
        length(List, Counter)
    ).

max_points(Player-Value, Count, Counter, MaxPoints) :-
    Multiplier is 2 ^ Counter,  % Calculate the multiplier based on the number of counters
    MaxPoints is Value * Count * Multiplier.  % Calculate the points

% Define a predicate to replace the value on the board at the specified row and column
empty_cell(OldBoard, Row1, Col1, Row2, Col2, NewBoard) :-
    ( Row1 == Row2 ->
        nth1(Row1, OldBoard, OldRow),
        replace_row(OldRow, Col1, Col2, NewRow),
        replace_list(OldBoard, Row1, NewRow, NewBoard)
     ;Col1 == Col2 ->
        transpose(OldBoard, TransposedBoard),
        nth1(Col1, TransposedBoard, OldRow),
        replace_row(OldRow, Row1, Row2, NewRow),
        replace_list(TransposedBoard, Col1, NewRow, TempBoard),
        transpose(TempBoard, NewBoard)
    ).


% Define a predicate to replace a range of values at the specified indices in a row
replace_row(Row, Col1, Col2, NewRow) :-
    length(Row, Length),
    findall(Y, (between(1, Length, I), (I >= Col1, I =< Col2 -> Y = ' - '; nth1(I, Row, Y))), NewRow).