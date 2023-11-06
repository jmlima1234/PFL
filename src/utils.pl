:- use_module(library(between)).

clear_data :-
    retractall(last_piece_removed(_)),
    retractall(name_of(_,_)),
    retractall(passed(_)),
    retractall(difficulty(_, _)),
    retractall(last_move(_)).


read_number(X):-
    repeat,
    read_line(Codes),
    (catch(number_codes(X, Codes), error(syntax_error(_), _), (write('\nInvalid input. Please enter a number.\n'), fail)) -> ! ; fail).

% get_option(+Min, +Max, +Context, -Value)
get_option(Min,Max,Context,Value):-
    format('~a between ~d and ~d: ', [Context, Min, Max]),
    nl,
    repeat,
    read_number(Value),
    (between(Min, Max, Value) -> ! ; write('\nInvalid input. Please enter a number between '), 
    write(Min), write(' and '), write(Max), nl, fail, format('~a between ~d and ~d: ', [Context, Min, Max]), nl).

get_move(Player, Col1-Row1-Col2-Row2, Piece) :-
    get_piece_or_pass(1, 6, Piece), % Get the piece value or 'pass'
    (Piece == 'pass' ->
        assertz(passed(Player))
    ;   
        player_value_pieces(Player, Number, _, Piece),
        (Number > 0 ->
            get_option(0, 9, '\nStart column', TempCol1),
            get_option(0, 9, '\nEnd column', TempCol2),
            get_option(0, 9, '\nStart row', TempRow1),
            get_option(0, 9, '\nEnd row', TempRow2),
            (TempCol1 > TempCol2 -> Col1 = TempCol2, Col2 = TempCol1 ; Col1 = TempCol1, Col2 = TempCol2),
            (TempRow1 < TempRow2 -> Row1 = TempRow2, Row2 = TempRow1 ; Row1 = TempRow1, Row2 = TempRow2)
        ;
            write('Invalid piece. You already played all the pieces with this value!'), nl, nl,
            get_move(Player, Col1-Row1-Col2-Row2, Piece)
        )
    ).


get_piece_or_pass(Min, Max, Value) :-
    write('Type "value" if you want to play or "pass" to pass: '),
    nl,
    read_line(String),    
    (String == "pass" ->
        Value = 'pass'
    ; String == "value" ->  
        get_option(Min, Max, '\nPiece value', TempValue),
        (TempValue == 5 ->
            write('Invalid piece value. You cannot choose a piece with value 5. Please try again.'), nl, nl,
            get_piece_or_pass(Min, Max, Value)
        ;
            Value = TempValue
        )
    ;   write('\nInvalid input. '),
        get_piece_or_pass(Min, Max, Value)
    ).

% put_piece(+Board,+Coordinate,+Piece,-NewBoard).
put_piece(Board, Col-Row, Piece, NewBoard) :-
    nth0(Row,Board,Line),
    replace(Col, Piece, Line, NewLine),
    replace(Row, NewLine, Board, NewBoard).

choose_piece_to_remove(PossibleMoves, Index) :-
    write('Your possible moves are: '), nl, nl,
    print_list(PossibleMoves, 1),
    length(PossibleMoves, Length),
    get_option(1, Length, '\nChoose a piece to remove', Index).

print_list([], _).
print_list([Row1-Col1-Col2-Row2-_-Value|T], Index) :-
    AdjustedRow1 is 11 - Row1,
    AdjustedRow2 is 11 - Row2,
    Tempcol is Col1 - 2,
    Tempcol2 is Col2 - 2,
    write(Index), write(' - '), 
    write('ColI = '), write(Tempcol), write(', '),
    write('ColF = '), write(Tempcol2), write(', '),
    write('RowI = '), write(AdjustedRow1), write(', '),
    write('RowF = '), write(AdjustedRow2), write(', '),
    write('Value = '), write(Value), nl,
    NewIndex is Index + 1,
    print_list(T, NewIndex).