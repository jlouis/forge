
%% Since Erlang doesn't support UTF8 atoms yet, we use this Latin1 marker as the
%% "Omega" symbol in Erlang at the moment.
-define(OMEGA, '«·Omega·»').

%% To implement a lens, implement the functions g and s given here, subject to the following rules:
%% (1) Assume R is the type of records, and X is the value returned by the lens
%% (2) g should have type R → X
%% (3) s should have type (X, R) → R
%% (4) The lens laws should be respected:
%% (4a) getput : forall R : s(g(R), R) == R
%% (4b) putget : forall R, X : g(s(X, R)) = X
%% And for very-well-behaved lenses
%% (4c) putput : forall R, X, Y, s(Y, s(X, R)) == s(Y, R)
-record(lens, {g, s}).
