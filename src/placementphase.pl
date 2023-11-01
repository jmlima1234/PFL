:- consult('data.pl').
:- consult('display.pl').


% Define a predicate to display the remaining pieces of each player
display_remaining_pieces :-
    current_player(Player),
    opponent_player(Opponent),
    display_remaining_pieces(Player),
    nl,
    display_remaining_pieces(Opponent),
    nl.

display_remaining_pieces(Player) :-
    findall(player_value_pieces(Player, Count, Size, Value), player_value_pieces(Player, Count, Size, Value), PlayerPieces),
    format('Remaining pieces for ~w pieces player:~n', [Player]),
    display_players_pieces(PlayerPieces).

display_players_pieces([]).
display_players_pieces([player_value_pieces(_, Count, Size, Value)|Rest]) :-
    format(' -~w pieces of value ~w (size ~w)~n', [Count, Value, Size]),
    display_players_pieces(Rest).

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
    (PieceOption =:= 5 -> write('Invalid piece option: 5'), nl, fail; true),
    between(1, 6, PieceOption),
    current_player(Player),
    player_value_pieces(Player, _, _, PieceOption), !.  % Use PieceOption directly

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
    assert(last_move(Row1-Col1-Col2-Row2)),
    display_board(NewBoard).

% Define a predicate to replace the value on the board at the specified row and column
replace(OldBoard, Row1, Col2, Row2, Col1, Value, NewBoard) :-
    ( Row1 == Row2 ->
        nth1(Row1, OldBoard, OldRow),
        current_player(Player),
        replace_row(OldRow, Col1, Col2, Value, Player, NewRow),
        replace_list(OldBoard, Row1, NewRow, NewBoard)
     ;Col1 == Col2 ->
        transpose(OldBoard, TransposedBoard),
        nth1(Col1, TransposedBoard, OldRow),
        current_player(Player),
        replace_row(OldRow, Row1, Row2, Value, Player, NewRow),
        replace_list(TransposedBoard, Col1, NewRow, TempBoard),
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
    sub_atom(Player, 0, 1, _, PlayerInitial),
    findall(Y, (between(1, Length, I), (I >= Col1, I =< Col2 -> atom_concat(PlayerInitial, '-', Temp), atom_concat(Temp, XAtom, Y); nth1(I, Row, Y))), NewRow).
