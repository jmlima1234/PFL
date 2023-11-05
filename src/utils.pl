:- use_module(library(between)).

clear_data :-
    retractall(name_of(_,_)),
    retractall(passed(_)),
    retractall(difficulty(_, _)),
    retractall(last_move(_)).


read_number(X):-
    repeat,
    read_line(Codes),
    (catch(number_codes(X, Codes), error(syntax_error(_), _), (write('\nInvalid input. Please enter a number.\n'), fail)) -> ! ; fail).

get_option(Min,Max,Context,Value):-
    format('~a between ~d and ~d: ', [Context, Min, Max]),
    nl,
    repeat,
    read_number(Value),
    (between(Min, Max, Value) -> ! ; write('\nInvalid input. Please enter a number between '), write(Min), write(' and '), write(Max), nl, fail, format('~a between ~d and ~d: ', [Context, Min, Max]), nl).

get_move(Player, Col1-Row1-Col2-Row2, Piece) :-
    get_piece_or_pass(1, 6, Piece), % Get the piece value or 'pass'
    (Piece == 'pass' ->
        assertz(passed(Player))
    ;   get_option(0, 9, '\nStart column', Col1),
        get_option(0, 9, '\nStart row', Row1),
        get_option(0, 9, '\nDestination column', Col2),
        get_option(0, 9, '\nDestination row', Row2)
    ).

get_piece_or_pass(Min, Max, Value) :-
    write('Type "value" if you want to play or "pass" to pass: '),
    nl,
    read_line(String),    
    (String == "pass" ->
        Value = 'pass'
    ; String == "value" ->  
        get_option(Min, Max, '\nPiece value', Value)
    ;   write('\nInvalid input. '),
        get_piece_or_pass(Min, Max, Value)
    ).

% put_piece(+Board,+Coordinate,+Piece,-NewBoard).
put_piece(Board, Col-Row, Piece, NewBoard) :-
    nth0(Row,Board,Line),
    replace(Col, Piece, Line, NewLine),
    replace(Row, NewLine, Board, NewBoard).


choose_piece_to_remove(PossibleMoves, Index) :-
    write('Your possible moves are: '), nl,
    print_list(PossibleMoves, 1),
    length(PossibleMoves, Length),
    get_option(1, Length, 'Choose a piece to remove', Index).

print_list([], _).
print_list([H|T], Index) :-
    write(Index), write(' - '), write(H), nl,
    NewIndex is Index + 1,
    print_list(T, NewIndex).
