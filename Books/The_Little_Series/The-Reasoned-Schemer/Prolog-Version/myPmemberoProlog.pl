% -*- Mode: Prolog -*-
:- module(myPmemberoProlog, [myPmembero/2, myPmemberoCan/2]).

% Implementation using build in [length]
myPmemberoHelper(X, [X | T], TailLength) :- length(T, TailLength).
myPmemberoHelper(X, [_ | T], TailLength) :- NewTailLength is TailLength - 1, NewTailLength >= 0, myPmemberoHelper(X, T, NewTailLength).

myPmemberoStartWith(X, L, Len) :- myPmemberoHelper(X, L, Len).
myPmemberoStartWith(X, L, Len) :- LenAdd1 is Len + 1, myPmemberoStartWith(X, L, LenAdd1).

myPmembero(X, L) :- myPmemberoStartWith(X, L, 0).

% Implementation using canonical list
myLengtho([], []).
myLengtho([_ | T1], [unit | T2]) :- myLengtho(T1, T2).

myPmemberoHelperCan(X, [X | T], Len) :- myLengtho(T, Len).
myPmemberoHelperCan(X, [_ | T], [unit | NewLen]) :- myPmemberoHelperCan(X, T, NewLen).

myPmemberoStartWithCan(X, L, Len) :- myPmemberoHelperCan(X, L, Len).
myPmemberoStartWithCan(X, L, Len) :- myPmemberoStartWithCan(X, L, [unit | Len]).

myPmemberoCan(X, L) :- myPmemberoStartWithCan(X, L, []).
