:- consult('data.pl').

% Define a predicate to display the board
display_board(Board) :-
    nl,
    display_separator,
    display_rows(Board),
    nl,
    display_current_player,
    %display_piece_to_move,
    %nl,
    %display_move_to,
    %nl,
    %display_remaining_pieces,
    display_counter_position,
    display_winner.

display_rows([]).
display_rows([Row|Rows]) :-
    display_row(Row),
    display_separator,
    display_rows(Rows).

display_separator :-
    write(' |----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----| '), nl.

display_row([]) :- nl.
display_row([Cell|Rest]) :-
    write(' | '), write(Cell),
    display_row(Rest).

% Predicate to display current player
display_current_player :-
    current_player(Player),
    format('Current player: ~w~n', [Player]),
    nl.

% Define a predicate to display the remaining pieces of each player
display_remaining_pieces :-
    current_player(Player),
    opponent_player(Opponent),
    display_remaining_pieces(Player),
    nl,
    display_remaining_pieces(Opponent),
    nl.

display_remaining_pieces(Player) :-
    findall(player_value_pieces(Player, Count, Size, Value), player_value_pieces(Player, Count, Size, Value), PlayerPieces),
    format('Remaining pieces for ~w pieces player:~n', [Player]),
    display_players_pieces(PlayerPieces).

display_players_pieces([]).
display_players_pieces([player_value_pieces(_, Count, Size, Value)|Rest]) :-
    format(' -~w pieces of value ~w (size ~w)~n', [Count, Size, Value]),
    display_players_pieces(Rest).

% Define a predicate to display the counter position
display_counter_position :-
    findall(score_counter(Player, Row, Col), score_counter(Player, Row, Col), CounterPositions),
    display_counter_position(CounterPositions).

display_counter_position([]).
display_counter_position([score_counter(Player, Row, Col)|Rest]) :-
    Score is Row*10 + Col,
    format('Counter position for ~w player: (~w, ~w)~n', [Player, Row, Col]),
    format('Score for ~w player: ~w~n~n', [Player, Score]),
    display_counter_position(Rest).

% Define a predicate to display what piece a player wants to move
display_piece_to_move :-
    write('Choose a piece (value) [3-7] to move:'),
    nl,
    write('|- '),
    nl.

% Define a predicate to display what col and row a player wants to move the piece to
display_move_to :-
    write('Choose a row [0-9] and a col [0-9] to move the piece to:'),
    nl,
    write('|- '),
    nl.

% Define a predicate to display the winner
display_winner :-
    write('No more tiles to remove. Game ends. \nPlayer Dark wins!').
