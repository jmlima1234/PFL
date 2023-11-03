:- consult(utils).

% Game header
isaac:-
    write('=================\n'),
    write('Welcome to Isaac!\n'),
    write('=================\n').

% Main menu
menu:-  
    write('Please select game mode:\n'),
    write('1 - Human vs. Human\n'),
    write('2 - Human vs. Bot\n'),
    write('3 - Bot vs. Bot\n'),
    nl.

% choose_difficulty(+Bot)
% Choose Bot difficulty (1 or 2)
choose_difficulty(Bot) :-
    format('Please select ~a status:\n', [Bot]),
    write('1 - Random\n'),
    write('2 - Hard\n'),
    get_option(1, 2, 'Difficulty', Option), !,
    asserta((difficulty(Bot, Option))),
    nl.


option(1):-
    clear,
    write('Human vs. Human\n'),
    nl,
    asserta((name_of(player1, 'Dark'))),
    asserta((name_of(player2, 'Light'))), !.
option(2):-
    clear,
    write('Human vs. Bot\n'),
    nl,
    asserta((name_of(player1, 'Dark'))),
    asserta((name_of(player2, 'Light'))), !,
    choose_difficulty(player2).
option(3):-
    clear,
    write('Bot vs. Bot\n'),
    nl,
    asserta((name_of(player1, 'Dark'))),
    asserta((name_of(player2, 'Light'))), !,
    choose_difficulty(player1),
    choose_difficulty(player2).


% set_mode/0
% Game mode choice
set_mode :-
    menu,
    get_option(1, 3, 'Mode', Option), !,
    option(Option).

init_state(Board):-
    board(_, Board).

% configuration(-GameState)
% Initialize GameState with Board, first Player, phase
configurations([Board,'Dark','Placement Phase']):-
    clear,
    clear_data,
    isaac,
    blank_lines,
    set_mode,
    init_state(Board).


