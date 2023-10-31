% name_of(+Player, -Name)
% Find the Players name
:- dynamic name_of/2.

% difficulty(+Bot,-Difficulty)
% Find the Bot difficulty
:- dynamic difficulty/2.

:- dynamic board/2.

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


:- dynamic player_value_pieces/4.

% Define facts for remaining pieces (player, number of pieces, size, value)
player_value_pieces('Light', 5, 3, 1).
player_value_pieces('Light', 4, 4, 2).
player_value_pieces('Light', 3, 5, 3).
player_value_pieces('Light', 2, 6, 4).
player_value_pieces('Light', 1, 7, 6).
player_value_pieces('Dark', 5, 3, 1).
player_value_pieces('Dark', 4, 4, 2).
player_value_pieces('Dark', 3, 5, 3).
player_value_pieces('Dark', 2, 6, 4).
player_value_pieces('Dark', 1, 7, 6).

% Define facts for score counters position
score_counter('Light', 0, 0).
score_counter('Dark', 0, 0).

:- dynamic played_pieces/1.
played_pieces([]).