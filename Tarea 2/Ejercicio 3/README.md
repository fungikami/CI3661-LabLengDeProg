# Sudoku Solver in Prolog

This program solves a sudoku puzzle using Prolog. The program receives as input a 9 × 9 board, with the following format:
- Each row will be on one line, separated by a newline.
- Values in the same row will be separated by a blank space.
- Each value can be a number from 1 to 9 or the period (.) character, which represents a blank space.

The program prints all possible solutions that are compatible with the numbers passed as input. A valid solution must be such that:
- Each row must have the numbers from 1 to 9, without repetitions.
- Each column must have the numbers from 1 to 9, without repetitions.
- Each 3 × 3 block that results from separating the board into thirds, in both directions, must have the numbers from 1 to 9, without repetition.

## How to run

```
swipl -s sudoku.pl -g main
```

or 

```
prolog -s sudoku.pl -g main
```
