:- consult('data.pl').
:- consult('display.pl').
:- consult('placementphase.pl').


% Define a predicate to start scoring phase
scoringphase_start(GameState, NewGameState) :-
    [_, Player, _] = GameState,
    valid_moves_SP(GameState, Player, PossibleMoves),
    % Adicionar condicaçao caso seja bot para escolher automaticamente
    choose_piece_to_remove(PossibleMoves, Index),
    nth1(Index, PossibleMoves, Move),
    (last_piece_removed(_-_-_-_-Player-_) ->
        retract(last_piece_removed(_-_-_-_-Player-_)),
        assert(last_piece_removed(Move))
    ;
        assert(last_piece_removed(Move))
    ),
    remove_piece(Index, PossibleMoves, GameState, NewGameState).

valid_moves_SP(GameState, Player, PossibleMoves) :-
    [_, _, _] = GameState,
    findall(Row1-Col1-Col2-Row2-PlayerP-Value, (last_move(Row1-Col1-Col2-Row2-PlayerP-Value), PlayerP == Player, valid_removal(Row1-Col1-Col2-Row2, 1, Player)), PossibleMoves).

valid_removal(Row1-Col1-Col2-Row2-_-Value, Valid, Player) :-
    (last_piece_removed(_-_-_-_-Player-ValueR) ->
        (Value >= ValueR -> Valid = 1; Valid = 0)
    ;
        Valid = 1
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

remove_piece(Index, PossibleMoves, GameState, NewGameState) :-
    nth1(Index, PossibleMoves, Move),
    removal_operation(Move, GameState),
    [Board, Player, Phase] = GameState,
    clear,
    [Row1-Col1-Col2-Row2-PlayerP-Value] = Move,
    retract(last_move(Row1-Col1-Col2-Row2-PlayerP-Value)),
    empty_cell(Board, Row1, Col1, Row2, Col2, NewBoard),
    retract(board(_, _)),
    assert(board(Board, NewBoard)),
    other_player(Player, NextPlayer),
    NewGameState = [NewBoard, NextPlayer, Phase],
    winning_condition(NewGameState).
    
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
    retract(player_score(Player, CurrentScore)),
    assert(player_score(Player, FinalScore)),

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

replace_row(Row, Col1, Col2, NewRow) :-
    length(Row, Length),
    findall(Y, (between(1, Length, I), (I >= Col1, I =< Col2 -> Y = ' - '; nth1(I, Row, Y))), NewRow).

winning_condition(GameState) :-
    [Board, Player, _] = GameState,
    other_player(Player, NextPlayer),
    valid_moves_SP(GameState, Player, PossibleMoves),
    player_score(Player, Score),
    player_score(NextPlayer, NextScore),
    (Score == 100 -> Winner = Player
    ; NextScore == 100 -> Winner = NextPlayer
    ; PossibleMoves == [] -> Winner = NextPlayer
    ; fail),
    GameState = [Board, Winner, 'game_over'].

game_over(GameState) :-
    [_, Winner, _] = GameState,
    display_board(GameState),
    format('No more tiles to remove. Game Over.~nPlayer ~w wins!', [Winner]).

choose_move(GameState, Player, Level, Move) :-
    valid_moves_SP(GameState, Player, PossibleMoves),
    (Level == 1 ->
        length(PossibleMoves, Length),
        random(1, Length, Index),
        nth1(Index, PossibleMoves, Move)
    ;
        choose_best_move(GameState, Player, PossibleMoves, Move)
    ).




