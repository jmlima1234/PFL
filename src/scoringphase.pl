:- consult('data.pl').
:- consult('display.pl').

is_piece_placed(Row, Col) :-
    % Iterate through the last moves
    findall(LastMove, last_move(LastMove), LastMoves),
    format('Last moves: ~w~n', [LastMoves]),
    member(Row1-Col1-Col2-Row2, LastMoves),
    % Check if the last move occupies the given row and column
    (occupies(Row1-Col1-Col2-Row2, Row, Col) -> 
        retract(last_move(Row1-Col1-Col2-Row2)),  % Remove the piece from last_move/1 if it's found
        write('true')
    ; 
        write("false")).  % Return false if the cell is not within a piece

% Helper function to check if a last move occupies a given row and column
occupies(Row1-Col1-Col2-Row2, Row, Col) :-
    between(Row1, Row2, Row),
    between(Col1, Col2, Col).