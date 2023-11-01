:- consult('data.pl').
:- consult('display.pl').
:- consult('placementphase.pl').
:- consult('utils.pl').

remove_piece(Row, Col) :-
    % Iterate through the last moves
    findall(LastMove, last_move(LastMove), LastMoves),
    check_all_moves(LastMoves, Row, Col).

remove_piece(Row1-Col1-Col2-Row2) :-
    clear,
    board(BoardId, OldBoard),
    Temprow is Row1 + 1,
    Temprow2 is Row2 + 1,
    Tempcol is Col1 + 1,
    Tempcol2 is Col2 + 1,
    empty_cell(OldBoard, Temprow, Tempcol, Temprow2, Tempcol2, NewBoard),
    retract(board(BoardId, OldBoard)),
    assert(board(BoardId, NewBoard)),
    display_board(NewBoard).

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
% Prolog

% Define a predicate to replace a range of values at the specified indices in a row
replace_row(Row, Col1, Col2, NewRow) :-
    length(Row, Length),
    findall(Y, (between(1, Length, I), (I >= Col1, I =< Col2 -> Y = ' - '; nth1(I, Row, Y))), NewRow).