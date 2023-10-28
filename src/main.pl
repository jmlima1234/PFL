:- use_module(library(lists)).
:- use_module(library(random)).
:- consult(display).
:- consult(data).

% validate_move(+Board,+CoordsOrigin,+CoordsDestination)
% Checks if the move is valid or not
validate_move(GameState, ColI-RowI,ColF-RowF) :-

