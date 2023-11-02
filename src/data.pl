% name_of(+Player, -Name)
% Find the Players name
:- dynamic name_of/2.

% difficulty(+Bot,-Difficulty)
% Find the Bot difficulty
:- dynamic difficulty/2.

% users passed in PP
:- dynamic passed/1.

:- dynamic board/2.

:- dynamic player_value_pieces/5.

:- dynamic last_move/1.

board(12,[
    ['  ' , ' 9 ', ' 8 ', ' 7 ', ' 6 ', ' 5 ', ' 4 ', ' 3 ', ' 2 ', ' 1 ',' 0 ', '    |'],
    ['90',' - ',' - ',' - ',' - ',' - ',' - ',' - ',' - ',' - ',' - ', '  0 |'],
    ['80',' - ',' - ',' - ',' - ',' - ',' - ',' - ',' - ',' - ',' - ', ' 10 |'],
    ['70',' - ',' - ',' - ',' - ',' - ',' - ',' - ',' - ',' - ',' - ', ' 20 |'],
    ['60',' - ',' - ',' - ',' - ',' - ',' - ',' - ',' - ',' - ',' - ', ' 30 |'],
    ['50',' - ',' - ',' - ',' - ',' - ',' - ',' - ',' - ',' - ',' - ', ' 40 |'],
    ['40',' - ',' - ',' - ',' - ',' - ',' - ',' - ',' - ',' - ',' - ', ' 50 |'],
    ['30',' - ',' - ',' - ',' - ',' - ',' - ',' - ',' - ',' - ',' - ', ' 60 |'],
    ['20',' - ',' - ',' - ',' - ',' - ',' - ',' - ',' - ',' - ',' - ', ' 70 |'],
    ['10',' - ',' - ',' - ',' - ',' - ',' - ',' - ',' - ',' - ',' - ', ' 80 |'],
    ['0 ',' - ',' - ',' - ',' - ',' - ',' - ',' - ',' - ',' - ',' - ', ' 90 |'],
    ['  ' , ' 0 ', ' 1 ', ' 2 ', ' 3 ', ' 4 ', ' 5 ', ' 6 ', ' 7 ', ' 8 ',' 9 ', '    |']
]).

% Define the current player
current_player('Dark').

% Define the opponent player
opponent_player('Light').

other_player(player1, player2).
other_player(player2, player1).


% Define facts for remaining pieces (player, piece, number of pieces, size, value)
player_value_pieces('Light',l1, 5, 3, 1).
player_value_pieces('Light',l2, 4, 4, 2).
player_value_pieces('Light',l3, 3, 5, 3).
player_value_pieces('Light',l4, 2, 6, 4).
player_value_pieces('Light',l6, 1, 7, 6).
player_value_pieces('Dark',d1, 5, 3, 1).
player_value_pieces('Dark',d2, 4, 4, 2).
player_value_pieces('Dark',d3, 3, 5, 3).
player_value_pieces('Dark',d4, 2, 6, 4).
player_value_pieces('Dark',d5, 1, 7, 6).

% Define facts for score counters position
score_counter('Light', 0, 0).
score_counter('Dark', 0, 0).
