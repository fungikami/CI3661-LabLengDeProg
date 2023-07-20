p(X) :- q(X, Y), not(q(X, X)), not(r(Y)).
q(X, a).
q(X, Y) :- not(r(X)), q(Y, a).
r(c).
