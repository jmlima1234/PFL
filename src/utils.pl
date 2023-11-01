:- use_module(library(between)).

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

% get_move(+Board,-Coordinate)
% Unifies Coordinate with a valid coordinate given by input within the Board
get_move(Col1-Row1-Col2-Row2, Piece):-
    get_option(1, 6, 'Choose your piece value', Piece),
    get_option(1, 10, 'Start column', Col1),
    get_option(1, 10, 'Start row', Row1),
    get_option(1, 10, 'Destination column', Col2),
    get_option(1, 10, 'Destination row', Row2).

% put_piece(+Board,+Coordinate,+Piece,-NewBoard).
put_piece(Board, Col-Row, Piece, NewBoard) :-
    nth0(Row,Board,Line),
    replace(Col, Piece, Line, NewLine),
    replace(Row, NewLine, Board, NewBoard).

% Helper function to check if a last move occupies a given row and column
occupies(Row1-Col1-Col2-Row2, Row, Col) :-
    between(Row1, Row2, Row),
    between(Col1, Col2, Col).

check_all_moves([], _, _) :-
    write('false'), !.

check_all_moves([Row1-Col1-Col2-Row2|Rest], Row, Col) :-
    % Check if the last move occupies the given row and column
    (occupies(Row1-Col1-Col2-Row2, Row, Col) ->
        retract(last_move(Row1-Col1-Col2-Row2)),  % Remove the piece from last_move/1 if it's found
        remove_piece(Row1-Col1-Col2-Row2), !
    ;
        check_all_moves(Rest, Row, Col)).  % Continue with the rest of the list

