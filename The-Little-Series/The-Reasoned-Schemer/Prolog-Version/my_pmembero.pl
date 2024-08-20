% -*- Mode: Prolog -*-
:- module(myPmemberoInProlog, [myPmembero/2]).

% Implementation using build in [length]
myPmemberoHelper(X, [X | T], TailLength) :- length(T, TailLength).
myPmemberoHelper(X, [_ | T], TailLength) :- NewTailLength is TailLength - 1, NewTailLength >= 0, myPmemberoHelper(X, T, NewTailLength).

myPmemberoStartWith(X, L, Len) :- myPmemberoHelper(X, L, Len).
myPmemberoStartWith(X, L, Len) :- LenAdd1 is Len + 1, myPmemberoStartWith(X, L, LenAdd1).

myPmembero(X, L) :- myPmemberoStartWith(X, L, 0).
