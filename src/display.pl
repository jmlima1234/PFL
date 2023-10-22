:- consult('data.pl').

% Define a predicate to display the board
display_board(Board) :-
    nl,
    display_rows(Board).

display_rows([]).
display_rows([Row|Rows]) :-
    display_row(Row),
    display_rows(Rows).

display_row([]) :- nl.
display_row([Cell|Rest]) :-
    write(' '), write(Cell), write(' |'),
    display_row(Rest).



