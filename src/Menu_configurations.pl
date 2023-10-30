:- consult(utils).

% Game header
Isaac:-
    write('=================\n'),
    write('Welcome to Isaac!\n'),
    write('=================\n').

% Main menu
menu:-  
    write('Please select game mode:\n'),
    write('1 - Human vs. Human\n'),
    write('2 - Human vs. Bot\n'),
    write('3 - Bot vs. Bot\n').

% choose_difficulty(+Bot)
% Choose Bot difficulty (1 or 2)
choose_difficulty(Bot) :-
    format('Please select ~a status:\n', [Bot]),
    write('1 - Random\n'),
    write('2 - Hard\n'),
    get_option(1, 2, 'Difficulty', Option), !,
    asserta((difficulty(Bot, Option))).


option(1):-
    write('Human vs. Human\n'),
    asserta((name_of(player1, 'Dark'))),
    asserta((name_of(player2, 'Light'))), !.
option(2):-
    write('Human vs. Bot\n'),
    asserta((name_of(player1, 'Dark'))),
    asserta((name_of(player2, 'Light'))), !,
    choose_difficulty(player2).
option(3):-
    write('Bot vs. Bot\n'),
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

% init_state(+Size,-Board)
init_state(12, Board):-
    board(12, Board).

% configuration(-GameState)
% Initialize GameState with Board, first Player, phase
configurations([Board,'Dark','Placement Phase']):-
    barca,
    set_mode,
    init_random_state,
    choose_player(Player),
    choose_board(Size), 
    init_state(Size, Board).