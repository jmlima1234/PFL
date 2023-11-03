:- consult('data.pl').
:- consult('display.pl').
:- consult('placementphase.pl').


check_all_moves([], _, _) :- !.

check_all_moves([Row1-Col1-Col2-Row2-Player-Value|Rest], Row, Col) :-
    % Check if the last move occupies the given row and column
    (occupies(Row1-Col1-Col2-Row2, Row, Col) ->
        pieces_same_line(Row1-Col1-Col2-Row2, Count),
        %format('Count: ~w~n', [Count]),
        counter_same_line(Row1-Col1-Col2-Row2, Counter),
        %format('Counter: ~w~n', [Counter]),
        max_points(Value, Count, Counter, MaxPoints),
        %format('MaxPoints: ~w~n', [MaxPoints]),
        handle_score_update(Player, MaxPoints),
        retract(last_move(Row1-Col1-Col2-Row2-Player-Value)),
        remove_piece(Row1-Col1-Col2-Row2,Player), !
    ;
        check_all_moves(Rest, Row, Col)).


handle_score_update(Player, NewScore) :-
    % Find the current score for the player
    player_score(Player, CurrentScore),

    % Calculate the difference between the new score and the current score
    ScoreDiff is NewScore - CurrentScore,

    (ScoreDiff >= 0 ->
        format('You have earned ~w points.~nHow many points do you want to add (where do you want to move your counter to)? ', [ScoreDiff]),
        read_number(PointsToAdd),
        (PointsToAdd >= 1, PointsToAdd =< ScoreDiff ->
            FinalScore is CurrentScore + PointsToAdd
        ;
            write('Invalid number of points. Please enter a number between 1 and the number of points you earned.'),
            handle_score_update(Player, NewScore)
        )
    ;
        FinalScore is NewScore
    ),

    % Update the player's score
    retract(player_score(Player, CurrentScore)),
    assert(player_score(Player, FinalScore)),

    % Update the player's score counter
    update_score_counter(Player, ScoreDiff).

update_score_counter(Player, ScoreDiff) :-

    score_counter(Player, Row, Col),

    (Player == 'Dark' ->
        NewRow is Row - ScoreDiff,
        NewCol is Col
    ;
        TotalPoints is Row * 10 + Col,
        NewTotalPoints is TotalPoints + ScoreDiff,
        NewRow is NewTotalPoints div 10,
        NewCol is NewTotalPoints mod 10
    ),

    retract(score_counter(Player, Row, Col)),
    assert(score_counter(Player, NewRow, NewCol)).

occupies(Row1-Col1-Col2-Row2, Row, Col) :-
    between(Row1, Row2, Row),
    between(Col1, Col2, Col).

remove_piece(Row, Col) :-
    findall(LastMove, last_move(LastMove), LastMoves),
    TempRow is Row + 1,
    TempCol is Col + 1,
    check_all_moves(LastMoves, TempRow, TempCol).

remove_piece(Row1-Col1-Col2-Row2,Player) :-
    %clear,
    board(BoardId, OldBoard),
    empty_cell(OldBoard, Row1, Col1, Row2, Col2, NewBoard),
    retract(board(BoardId, OldBoard)),
    assert(board(BoardId, NewBoard)),
    display_board(NewBoard,Player).

pieces_same_line(Row1-Col1-Col2-Row2, Count) :-
    (Row1 == Row2 -> 
        findall(_, (last_move(TempRow1-_-_-TempRow2-_-_), TempRow1 =< Row1, Row1 =< TempRow2), List),
        length(List, TempCount),
        Count is TempCount - 1
    ;Col1 == Col2 ->
        findall(_, (last_move(_-TempCol1-TempCol2-_-_-_), TempCol1 =< Col1, Col1 =< TempCol2), List),
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

max_points(Value, Count, Counter, MaxPoints) :-
    CMultiplier is 2 ^ Counter,  % Calculate the multiplier based on the number of counters
    (Count == 0 ->
        MaxPoints is Value*Count  % If Count is 0, MaxPoints is the Value of the piece
    ;
        MaxPoints is Value * Count * CMultiplier  % Calculate the points
    ).

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