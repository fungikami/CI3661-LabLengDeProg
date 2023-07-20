p(f(X)) :- q(X, Y), r(Y).
q(g(X, Y), Z) :- r(X), r(Z), q(f(Z), a).
q(X, a).
r(f(f(b))).
r(c).

model :-
    r(c),
    r(f(f(b))),
    q(f(c), a),
    q(g(f(f(b)), c), c),
    p(f(g(f(f(b)), c))).
