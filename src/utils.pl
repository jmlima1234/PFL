:- use_module(library(between)).

clear_data :-
    retractall(name_of(_,_)),
    retractall(passed(_)),
    retractall(difficulty(_, _)),
    retractall(last_move(_)).


read_number(X):-
    read_number_aux(X,0).
read_number_aux(X,Acc):- 
    get_code(C),
    between(48, 57, C), !,
    Acc1 is 10*Acc + (C - 48),
    read_number_aux(X,Acc1).
read_number_aux(X,X).

% get_option(+Min,+Max,+Context,-Value)
% Unifies Value with the value given by user input between Min and Max when asked about Context
get_option(Min,Max,Context,Value):-
    format('~a between ~d and ~d: ', [Context, Min, Max]),
    nl,
    repeat,
    read_number(Value),
    between(Min, Max, Value), !.

get_move(Player, Col1-Row1-Col2-Row2, Piece) :-
    get_piece_or_pass(1, 6, 'Do you want to type a value', Piece), % Get the piece value or 'pass'
    (Piece == 'pass' ->
        assertz(passed(Player))
    ;   get_option(0, 9, 'Start column', Col1),
        get_option(0, 9, 'Start row', Row1),
        get_option(0, 9, 'Destination column', Col2),
        get_option(0, 9, 'Destination row', Row2)
    ).

get_piece_or_pass(Min,Max,Context,Value) :-
    format(' ~a between ~d and ~d or type "pass"? ', [Context, Min, Max]),
    nl,
    read_line(String),    
    (String == "pass" ->
        Value = 'pass'
    ; String == "value" ->  
        format('~a between ~d and ~d: ', [Context, Min, Max]),
        nl,
        repeat,
        read_number(Value),
        between(Min, Max, Value), !
    ;   write('Invalid input. Please enter a numeric piece value or "pass".\n'),
        get_piece_or_pass(1, 6, 'Do you want to type a value', Piece)
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
