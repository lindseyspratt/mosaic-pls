:- module(geometry, [in_interval/3, in_square/5]).

in_square(X, Y, Left, Top, Size) :-
    in_interval(X, Left, Left+Size),
    in_interval(Y, Top, Top+Size).

in_interval(V, Low, High) :-
    V >= Low,
    V =< High.

