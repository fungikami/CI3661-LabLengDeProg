% Church definition: 
% *→ 0
% up(*) → 1
% up(up(*)) → 2
% up(up(up(*))) → 3

% Church calculator
church(0, *).
church(N, up(X)) :- N > 0, N1 is N - 1, church(N1, X). 

% suma/3, true if 3rd argument is the sum of the first two
% Example:
% ?- suma(up(up(*)), up(*), X).
% X = up(up(up(*))).
suma(X, *, X).
suma(X, up(Y), up(Z)) :- suma(X, Y, Z).

% suma(*, X, X).
% suma(up(X), Y, up(Z)) :- suma(X, Y, Z).

% resta/3, true if 3rd argument is the difference of the first two
% If the first argument is less than the second, it's undefined
% Example:
% ?- resta(up(up(*)), up(*), X).
% X = up(*).
resta(X, *, X).
resta(up(X), up(Y), Z) :- resta(X, Y, Z).

% producto/3, true if 3rd argument is the product of the first two
% Example:
% ?- producto(up(up(up(*))), up(up(*)), X).
% X = up(up(up(up(up(up(*)))))).
producto(*, _, *).
producto(up(X), Y, Z) :- producto(X, Y, W), suma(W, Y, Z).

% producto2(*, _, *).
% producto2(up(X), Y, Z) :- producto2(X, Y, W), suma(Y, W, Z).

