:- consult('data.pl').
:- consult('display.pl').
:- consult('placementphase.pl').
:- consult('utils.pl').


% Define a predicate to start scoring phase
scoringphase_start(GameState, NewGameState) :-
    [_, Player, _] = GameState,
    valid_moves_SP(GameState, Player, PossibleMoves),
    (difficulty(Player, Level) ->
        (Level == 1 ->
            choose_move(GameState, Player, Level, Move),
            nth1(Index, PossibleMoves, Move)
        ;
            Level == 2 ->
            choose_move(GameState, Player, Level, Move),
            nth1(Index, PossibleMoves, Move)
        )
    ;
        choose_piece_to_remove(PossibleMoves, Index),
        nth1(Index, PossibleMoves, Move)
    ),
    (last_piece_removed(_-_-_-_-Player-_) ->
        retract(last_piece_removed(_-_-_-_-Player-_)),
        assert(last_piece_removed(Move))
    ;
        assert(last_piece_removed(Move))
    ),
    remove_piece(Index, PossibleMoves, GameState, TempNewGameState),
    winning_condition(TempNewGameState, NewGameState).

valid_moves_SP(GameState, Player, PossibleMoves) :-
    [_, _, _] = GameState,
    findall(Row1-Col1-Col2-Row2-PlayerP-Value, (last_move(Row1-Col1-Col2-Row2-PlayerP-Value), PlayerP == Player, valid_removal(Row1-Col1-Col2-Row2-_-Value, 1, Player)), PossibleMoves).


valid_removal(Row1-Col1-Col2-Row2-_-Value, Valid, Player) :-

    (last_piece_removed(_-_-_-_-Player-ValueR) ->
        (Value >= ValueR -> Valid = 1; Valid = 0)
    ;
        Valid = 1
    ),

    score_counter('Light', Col3, Row3),
    score_counter('Dark', Col4, Row4),
    TempRow3 is 11 - Row3,
    TempRow4 is 11 - Row4,
    TempCol3 is Col3 + 2,
    TempCol4 is Col4 + 2,

    (Row1 == Row2 ->
        (Row1 == TempRow4 ->
            (Col1 =< TempCol4, TempCol4 =< Col2 -> Valid = 0; true)
        ;
        Row1 == TempRow3 ->
            (Col1 =< TempCol3, TempCol3 =< Col2 -> Valid = 0; true)
        ;
            true
        )
    ;Col1 == Col2 ->
        (Col1 == TempCol4 ->
            (Row1 =< TempRow4, TempRow4 =< Row2 -> Valid = 0; true)
        ;
        Col1 == TempCol3 ->
            (Row1 =< TempRow3, TempRow3 =< Row2 -> Valid = 0; true)
        ;
            true
        )
    ).

remove_piece(Index, PossibleMoves, GameState, NewGameState) :-
    nth1(Index, PossibleMoves, Row1-Col1-Col2-Row2-PlayerP-Value),
    removal_operation(Row1-Col1-Col2-Row2-PlayerP-Value, GameState),
    [Board, Player, Phase] = GameState,
    clear,
    empty_cell(Board, Row1, Col1, Row2, Col2, NewBoard),
    retract(last_move(Row1-Col1-Col2-Row2-PlayerP-Value)),
    retract(board(_, _)),
    assert(board(Board, NewBoard)),
    NewGameState = [NewBoard, Player, Phase].
    
removal_operation(Row1-Col1-Col2-Row2-_-Value, GameState) :-
    [_, Player, _] = GameState,
    pieces_same_line(Row1-Col1-Col2-Row2, Count),
    counter_same_line(Row1-Col1-Col2-Row2, Counter),
    max_points(Value, Count, Counter, MaxPoints),
    handle_score_update(Player, MaxPoints).

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
        findall(_, (score_counter(_,_,Row), 
        TempRow is 11 - Row, TempRow == Row1), List),
        length(List, Counter)
    ;Col1 == Col2 ->
        findall(_, (score_counter(_,Col,_),
        TempCol is Col + 2, TempCol == Col1), List),
        length(List, Counter)
    ).

max_points(Value, Count, Counter, MaxPoints) :-
    CMultiplier is 2 ^ Counter,  % Calculate the multiplier based on the number of counters
    (Count == 0 ->
        MaxPoints is Value*CMultiplier  % If Count is 0, MaxPoints is the Value of the piece
    ;
        MaxPoints is Value*Count*CMultiplier  % Calculate the points
    ).
    
handle_score_update(Player, NewScore) :-
    % Find the current score for the player
    player_score(Player, CurrentScore),

    % Calculate the difference between the new score and the current score
    ScoreDiff is NewScore,

    (ScoreDiff >= 0 ->
        (difficulty(Player, Level) ->
            (Level == 1 ->
                random(0, ScoreDiff, PointsToAdd)
            ;
                PointsToAdd is ScoreDiff
            )
        ;
            format('~nYou have earned ~w points.~nYou can choose from 1 to ~w points. How many points do you want to add (where do you want to move your counter to)? ', [ScoreDiff, ScoreDiff]),
            read_number(PointsToAdd),
            (PointsToAdd >= 1, PointsToAdd =< ScoreDiff ->
                true
            ;
                write('Invalid number of points. Please enter a number between 1 and the number of points you earned.'),
                handle_score_update(Player, NewScore)
            )
        ),
        FinalScore is CurrentScore + PointsToAdd
    ;
        FinalScore is NewScore
    ),
    retract(player_score(Player, CurrentScore)),
    assert(player_score(Player, FinalScore)),

    update_score_counter(Player, PointsToAdd).


update_score_counter(Player, FinalScore) :-
    score_counter(Player, Col, Row),

    (Player == 'Dark' ->
        NewCol is Col - FinalScore,
        (NewCol < 0 -> 
            RowsDown is abs(NewCol // 10) + 1,
            AdjustedCol is (NewCol mod 10)
        ;
            RowsDown = 0,
            AdjustedCol = NewCol
        ),
        NewRow is Row - RowsDown
    ;
        TotalPoints is Row * 10 + Col,
        NewTotalPoints is TotalPoints + FinalScore,
        NewRow is NewTotalPoints div 10,
        AdjustedCol is NewTotalPoints mod 10
    ),

    retract(score_counter(Player, Col, Row)),
    assert(score_counter(Player, AdjustedCol, NewRow)).

empty_cell(OldBoard, Row1, Col1, Row2, Col2, NewBoard) :-
    ( Row1 == Row2 ->
        nl,
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

replace_row(Row, Col1, Col2, NewRow) :-
    length(Row, Length),
    findall(Y, (between(1, Length, I), (I >= Col1, I =< Col2 -> Y = ' - '; nth1(I, Row, Y))), NewRow).

winning_condition(GameState, NewGameState) :-
    [Board, Player, Phase] = GameState,
    player_score(Player, Score),
    other_player(Player, NextPlayer),
    valid_moves_SP(GameState, NextPlayer, PossibleMoves),
    valid_moves_SP(GameState, Player, PossibleMoves2),
    length(PossibleMoves, Length),
    length(PossibleMoves2, Length2),
    player_score(NextPlayer, NextPlayerScore),

    (Score >= 100 ->
        NewGameState = [Board, Player, 'game_over']
    ; NextPlayerScore >= 100 ->
        NewGameState = [Board, NextPlayer, 'game_over']
    ; Length == 0, Length2 > 0 ->
        NewGameState = [Board, Player, Phase]
    ; Length > 0, Length2 == 0 ->
        NewGameState = [Board, NextPlayer, Phase]
    ; Length == 0, Length2 == 0 ->
        (Score > NextPlayerScore ->
            NewGameState = [Board, Player, 'game_over']
        ; Score < NextPlayerScore ->
            NewGameState = [Board, NextPlayer, 'game_over']
        ; Score == NextPlayerScore ->
            findall(Product, (player_value_pieces(Player, Count, Size, _), Product is Count * Size), PlayerProducts),
            findall(Product, (player_value_pieces(NextPlayer, Count, Size, _), Product is Count * Size), NextPlayerProducts),
            sumlist(PlayerProducts, PlayerTotalProduct),
            sumlist(NextPlayerProducts, NextPlayerTotalProduct),
            (PlayerTotalProduct > NextPlayerTotalProduct ->
                NewGameState = [Board, Player, 'game_over']
            ; PlayerTotalProduct < NextPlayerTotalProduct ->
                NewGameState = [Board, NextPlayer, 'game_over']
            ; NewGameState = [Board, Player, 'game_over']
            )
        )
    ; NewGameState = [Board, NextPlayer, Phase]
    ).
   

game_over(GameState, Winner) :-
    [_, Winner, _] = GameState,
    display_board(GameState).

choose_move(GameState, Player, Level, Move) :-
    valid_moves_SP(GameState, Player, PossibleMoves),
    (Level == 1 ->
        length(PossibleMoves, Length),
        (Length == 1    ->  
            Index = 1
        ;
            random(1, Length, Index)
        ),
        nth1(Index, PossibleMoves, Move)
    ;
        find_best_move(PossibleMoves, Move)
    ).

% Evaluate each move and return a list of pairs (Score-Move)
evaluate_moves([], []).
evaluate_moves([Move|Moves], [Score-Move|ScoredMoves]) :-
    evaluate_move(Move, Score),
    evaluate_moves(Moves, ScoredMoves).

% Find the move with the maximum score
% If there are multiple moves with the same maximum score, select the one with the smallest size
find_best_move(PossibleMoves, BestMove) :-
    evaluate_moves(PossibleMoves, ScoredMoves),
    max_member(_-_, ScoredMoves),
    findall(Score-Size-Move, (member(Score-Move, ScoredMoves), move_size(Move, Size)), ScoredSizedMoves),
    sort(ScoredSizedMoves, SortedScoredSizedMoves),
    last(SortedScoredSizedMoves, _-_-BestMove).

move_size(_-_-_-_-_-Value, Size) :-
    player_value_pieces(_, _, Size, Value).

% Evaluate a move based on the value of the piece being removed and the points gained
evaluate_move(Row1-Col1-Col2-Row2-_-Value, Score) :-
    pieces_same_line(Row1-Col1-Col2-Row2, Count),
    counter_same_line(Row1-Col1-Col2-Row2, Counter),
    max_points(Value, Count, Counter, MaxPoints),
    Score is MaxPoints.