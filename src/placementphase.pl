:- use_module(library(lists)).
:- use_module(library(random)).
:- consult('data.pl').
:- consult('display.pl').
:- consult('utils.pl').


% Define a predicate to display the remaining pieces of each player
display_remaining_pieces(Player) :-
    display_remaining_pieces_list(Player),
    nl,
    other_player(Player, Opponent),
    display_remaining_pieces_list(Opponent),
    nl.

display_remaining_pieces_list(Player) :-
    findall(Value-Count-Size, player_value_pieces(Player, Count, Size, Value), PlayerPieces),
    keysort(PlayerPieces, SortedPlayerPieces),
    format('Remaining pieces for ~w pieces player:~n', [Player]),
    display_players_pieces(SortedPlayerPieces).

display_players_pieces([]).
display_players_pieces([Value-Count-Size|Rest]) :-
    format(' -~w pieces of value ~w (size ~w)~n', [Count, Value, Size]),
    display_players_pieces(Rest).

choose_move_PP(GameState, Col1-Row1-Col2-Row2, NewGameState) :-
    [Board, Player, _] = GameState,
    (difficulty(Player, Level) ->
        choose_move_PP(GameState, Player, Level, Move),
        [Col1,Row1,Col2,Row2,Size,Piece] = Move,
        player_value_pieces(Player, Number, Size, Piece),
        (Number == 0 -> 
            choose_move_PP(GameState, ColI-RowI-ColF-RowF, NewGameState)
        ;validate_move_PP(GameState,Col1-Row1,Col2-Row2, Size) ->
            place_piece(GameState,Piece,Row1,Col1,Col2,Row2,NewGameState)
        ;
            choose_move_PP(GameState, ColI-RowI-ColF-RowF, NewGameState)
        )
    ;
        get_move(Player, Col1-Row1-Col2-Row2, PieceOption),
        (PieceOption == 5 -> 
            write('Invalid piece option: 5'), nl, 
            choose_move(GameState, Col1-Row1-Col2-Row2, NewGameState)
        ; PieceOption \= 'pass' ->
            AdjustedRow1 is 11 - Row1,
            AdjustedRow2 is 11 - Row2,
            Tempcol is Col1 + 2,
            Tempcol2 is Col2 + 2,
            player_value_pieces(Player, _, Size, PieceOption),
            (validate_move_PP(GameState, Tempcol-AdjustedRow1,Tempcol2-AdjustedRow2, Size) ->
                write('Valid move!'), nl,
                place_piece(GameState, PieceOption, AdjustedRow1, Tempcol, Tempcol2, AdjustedRow2, NewGameState)
            ; 
                choose_move(GameState, Col1-Row1-Col2-Row2, NewGameState)
            )
        ; 
            other_player(Player, NextPlayer),
            (passed(NextPlayer) ->
                NewGameState = [Board, NextPlayer, 'Scoring Phase'],
                clear,
                write('Placement phase is over! Going for the scoring phase!'), nl
            ;
                clear,
                NewGameState = [Board, NextPlayer, 'Placement Phase']
            )
        ) 
    ). 
choose_move_PP(GameState, Player, 1, Move):-
    random(1,3,Direction),
    (Direction == 1 ->
        random(2,11,RandomRow),
        random(1,5,RandomValue),
        (RandomValue == 5 ->
            RandomValue2 is RandomValue +1;
            RandomValue2 is RandomValue
        ),
        player_value_pieces(Player, Number, Size, RandomValue2),
        random(2,11,RandomColI),
        RandomColF is RandomColI + Size - 1,
        Move = [RandomColI,RandomRow,RandomColF,RandomRow,Size,RandomValue2]
    ;
        random(2,11,RandomCol),
        random(1,5,RandomValue),
        (RandomValue == 5 ->
            RandomValue2 is RandomValue +1;
            RandomValue2 is RandomValue
        ),
        player_value_pieces(Player, _, Size, RandomValue2),
        random(2,11,RandomRowI),
        RandomRowF is RandomRowI + Size - 1,
        Move = [RandomCol,RandomRowI,RandomCol,RandomRowF,Size,RandomValue2]
    ).

% Define a predicate to select a piece option with active selection
pieceoption(PieceOption) :-
    (PieceOption =:= 5 -> write('Invalid piece option: 5'), nl, fail; true),
    between(1, 6, PieceOption),
    current_player(Player),
    player_value_pieces(Player, _, _, PieceOption), !. 

place_piece(GameState, PieceOption, Row1, Col1, Col2, Row2, NewGameState) :-
    clear,
    [Board, Player, Phase] = GameState,
    (PieceOption =:= 5 -> Value is 6 ; Value is PieceOption),
    player_value_pieces(Player, Count, Size, Value),
    NewCount is Count - 1,
    retract(player_value_pieces(Player, Count, Size, Value)),
    assert(player_value_pieces(Player, NewCount, Size, Value)),
    replace(Board, Row1, Col2, Row2, Col1, Value, NewBoard, Player),
    retract(board(_, _)),
    assert(board(Board, NewBoard)),
    assert(last_move(Row1-Col1-Col2-Row2-Player-Value)),
    other_player(Player, NextPlayer),
    (passed(NextPlayer) ->
        NewGameState = [NewBoard, Player, Phase]
    ;   NewGameState = [NewBoard, NextPlayer, Phase]
    ).

% Define a predicate to replace the value on the board at the specified row and column
replace(OldBoard, Row1, Col2, Row2, Col1, Value, NewBoard, Player) :-
    ( Row1 == Row2 ->
        nth1(Row1, OldBoard, OldRow),
        replace_row(OldRow, Col1, Col2, Value, Player, NewRow),
        replace_list(OldBoard, Row1, NewRow, NewBoard)
     ;Col1 == Col2 ->
        transpose(OldBoard, TransposedBoard),
        nth1(Col1, TransposedBoard, OldRow),
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