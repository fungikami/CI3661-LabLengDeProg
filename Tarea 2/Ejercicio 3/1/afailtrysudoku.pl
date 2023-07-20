% -------------------- SOLVE SUDOKU WITHOUT CLPFD --------------------




% ---------------------------- READ BOARD ----------------------------
% Read a list of 9 lists of 9 numbers
read_board(Board) :-
    % Print instructions
    write('Sudoku Solver'), nl,
    write('Enter a sudoku board:'), nl,
    write('- Enter numbers between 1 and 9.'), nl,
    write('- Use a dot (.) for empty cells.'), nl,
    write('- Use a space between each number.'), nl,
    write('- Use a new line for each row.'), nl, nl,
    write('Enter the board:'), nl,
    length(Board, 9),
    maplist(read_row, Board),
    nl.

% Read a list of 9 numbers
read_row(Row) :-
    length(Row, 9),
    maplist(read_cell, Row).

% Read a number: either a dot or a digit
read_cell(Cell) :-
    get_char(C),
    (   C = '.'
    ->  Cell = _
    ;   number_codes(Cell, [C])
    ),
    get_char(_).

% ---------------------------- PRINT BOARD ----------------------------
% Print each row of the board separated by a new line
print_lists([]).
print_lists([H|T]) :- print_list(H), nl, print_lists(T).

% Print each number from a row separated by a space
print_list([]).
print_list([H|[]]) :- writef('%w', [H]).
print_list([H|T]) :- writef('%w ', [H]), print_list(T).

% Print all solutions
print_all_solutions([]).
print_all_solutions([H|T]) :-
    write('Solution:'),
    nl,
    print_lists(H),
    nl,
    print_all_solutions(T).

% ---------------------------- EXAMPLES ----------------------------
% Example sudoku board with 2 possible solutions
twoSol([
    [2, 9, 5, 7, 4, 3, 8, 6, 1],
    [4, 3, 1, 8, 6, 5, 9, 0, 0],
    [8, 7, 6, 1, 9, 2, 5, 4, 3],
    [3, 8, 7, 4, 5, 9, 2, 1, 6],
    [6, 1, 2, 3, 8, 7, 4, 9, 5],
    [5, 4, 9, 2, 1, 6, 7, 3, 8],
    [7, 6, 3, 5, 2, 4, 1, 8, 9],
    [9, 2, 8, 6, 7, 1, 3, 5, 4],
    [1, 5, 4, 9, 3, 8, 6, 0, 0]
]).

example :-
    twoSol(Board),
    findall(Solution, solve(Board, Solution), Solutions),
    print_all_solutions(Solutions).

% ---------------------------- MAIN ----------------------------
main :-
    read_board(Board),
    findall(Solution, solve(Board, Solution), Solutions),
    print_all_solutions(Solutions).
