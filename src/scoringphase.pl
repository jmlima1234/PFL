:- consult('data.pl').
:- consult('display.pl').
:- consult('placementphase.pl').


% Define a predicate to start scoring phase
scoringphase_start(GameState, NewGameState) :-
    choose_piece_to_remove(Row, Col),
    remove_piece(Row, Col, GameState, NewGameState).


check_all_moves([], _, _, _, _, _, _) :- !.

check_all_moves([Row1-Col1-Col2-Row2-PlayerP-Value|Rest], Row, Col, Player, GameState, Board, NewGameState) :-
    % Check if the last move occupies the given row and column
    (occupies(Row1-Col1-Col2-Row2, Row, Col) ->
        list_removed_pieces_player(Player, RemovedPieces),
        (PlayerP == Player ->
            handle_player_piece(RemovedPieces, Row1-Col1-Col2-Row2-PlayerP-Value, GameState, Board, NewGameState)
        ;
            write('You cannot remove your opponent piece. Please choose another piece to remove.'), nl,
            scoringphase_start(GameState, Board, NewGameState)
        )
    ;
        check_all_moves(Rest, Row, Col, Player, GameState, Board, NewGameState)
    ).

handle_player_piece([], Move, GameState, Board, NewGameState) :-
    removal_operation(Move, GameState, NewGameState),
    scoringphase_start(GameState, Board, NewGameState).

handle_player_piece(_, Move, GameState, Board, NewGameState) :-
    [_, Player, _] = GameState,
    valid_removal(Move, Valid, Player),
    Move = _-_-_-_-PlayerP-_,
    (Valid == 1 ->
        retract(last_piece_removed(_-_-_-_-PlayerP-_)),
        removal_operation(Move, GameState, NewGameState),
        scoringphase_start(GameState, Board, NewGameState)
    ;
        write('You cannot remove a piece with less size than the one you removed before.'), nl,
        scoringphase_start(GameState, Board, NewGameState)
    ).

list_removed_pieces_player(Player, RemovedPieces) :-
    findall(Row1-Col1-Col2-Row2-PlayerP-Value, (last_piece_removed(Row1-Col1-Col2-Row2-PlayerP-Value), PlayerP == Player), RemovedPieces).

valid_removal(Row1-Col1-Col2-Row2, Valid, Player) :-
    last_move(Row1-Col1-Col2-Row2-PlayerP-Value),
    (last_piece_removed(_-_-_-_-_-ValueR) ->
        (Player == PlayerP, Value >= ValueR -> Valid = 1; Valid = 0)
    ;
        (Player == PlayerP -> Valid = 1; Valid = 0)
    ),

    score_counter('Light', Row3, Col3),
    score_counter('Dark', Row4, Col4),

    (Row1 == Row2 ->
        (Col1 =< Col3, Col3 =< Col2 -> Valid = 0; true),
        (Col1 =< Col4, Col4 =< Col2 -> Valid = 0; true)
    ;
        (Row1 =< Row3, Row3 =< Row2 -> Valid = 0; true),
        (Row1 =< Row4, Row4 =< Row2 -> Valid = 0; true)
    ).

removal_operation(Row1-Col1-Col2-Row2-PlayerP-Value, GameState, NewGameState) :-
    [_, Player, _] = GameState,
    pieces_same_line(Row1-Col1-Col2-Row2, Count),
    counter_same_line(Row1-Col1-Col2-Row2, Counter),
    max_points(Value, Count, Counter, MaxPoints),
    handle_score_update(Player, MaxPoints),
    assert(last_piece_removed(Row1-Col1-Col2-Row2-PlayerP-Value)),
    retract(last_move(Row1-Col1-Col2-Row2-PlayerP-Value)),
    remove_piece(Row1-Col1-Col2-Row2, GameState, NewGameState), !.

check_possible_removal(GameState, PossibleMoves) :-
    [_, Player, _] = GameState,
    findall(Row1-Col1-Col2-Row2-PlayerP-Value, (last_move(Row1-Col1-Col2-Row2-PlayerP-Value), PlayerP == Player, valid_removal(Row1-Col1-Col2-Row2, 1, Player)), PossibleMoves).


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

remove_piece(Row, Col, GameState, NewGameState) :-
    [Board, Player, _] = GameState,
    findall(LastMove, last_move(LastMove), LastMoves),
    TempRow is Row + 1,
    TempCol is Col + 1,
    check_all_moves(LastMoves, TempRow, TempCol, Player, GameState, Board, NewGameState).

remove_piece(Row1-Col1-Col2-Row2, GameState, NewGameState) :-
    [Board, Player, Phase] = GameState,
    clear,
    empty_cell(Board, Row1, Col1, Row2, Col2, NewBoard),
    retract(board(_, _)),
    assert(board(Board, NewBoard)),
    other_player(Player, NextPlayer),
    NewGameState = [NewBoard, NextPlayer, Phase],
    winning_condition(NewGameState).

winning_condition(NewGameState) :-
    [Board, Player, _] = NewGameState,
    other_player(Player, NextPlayer),
    check_possible_removal(NewGameState, PossibleMoves),
    player_score(Player, Score),
    player_score(NextPlayer, NextScore),
    (Score == 100 -> Winner = Player
    ; NextScore == 100 -> Winner = NextPlayer
    ; PossibleMoves == [] -> Winner = NextPlayer
    ; fail),
    NewGameState = [Board, Winner, 'game_over'].

% Predicate to handle the game over state
game_over(GameState) :-
    [_, Winner, _] = GameState,
    display_board(GameState),
    format('No more tiles to remove. Game Over.~nPlayer ~w wins!', [Winner]).

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