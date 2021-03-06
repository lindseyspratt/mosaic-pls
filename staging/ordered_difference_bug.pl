:- module(ordered_difference, [test/1]).

:- use_module('../src/components').

% ordered_difference([a,c,e], [b,c,d], R).
% There appears to be a bug in handling of term comparison.
% In certain (non-debug) cases this ordered_difference/3 fails
% to recognize that an element in the first list is also
% in the second list.

ordered_difference([], _, []) :- !.
ordered_difference(L, [], L) :- !.
ordered_difference([H|T], [HS|TS], Res) :-
    (H @< HS
      -> TN = T,
         Res = [H|ResN],
         TSN = [HS|TS]
    ;
    H @> HS
      -> TN = [H|T],
         Res = ResN,
         TSN = TS
    ;
    H = HS
      -> TN = T,
         Res = ResN,
         TSN = TS
    ),
    ordered_difference(TN, TSN, ResN).

check_difference(Minuend, Subtrahend, Difference) :-
    (member(X, Subtrahend),
     member(X, Difference)
      -> findall(Y, (member(Y, Subtrahend),member(Y, Difference)), Duplicated)
    ;
     Duplicated = []
    ),
    (member(X, Minuend),
     \+ member(X,Subtrahend ),
     \+ member(X, Difference)
      -> findall(Y, (member(X, Minuend),\+member(Y, Subtrahend),\+member(Y, Difference)), Lost)
    ;
     Lost = []
    ),
    ((Duplicated \= [];Lost \= [])
      -> writeln(bad_difference(Lost, Duplicated, Minuend, Subtrahend, Difference))
    ;
    true
    ).

% testing

% yield.

test(C) :-
    S =
        t(1-1+1,[1/1,1/2,1/3,1/4],
          t,
          t(1/1,[1-1+1],
            t(2-1+1,[2/1,2/2,2/3,2/4],
              t,
              t(3-1+1,[3/1,3/3,3/4],
                t,
                t(3-2+2,[3/2],
                  t,
                  t(4-1+1, [4/1,4/2,4/4],
                    t,
                    t(4-2+2,[4/3],
                      t,
                      t(5-1+1,[5/1,5/2],
                        t,
                        t(5-2+2,[5/3,5/4],
                          t,
                          t(6-1+2,[6/1,6/2],
                            t,
                            t(6-2+1,[6/3,6/4],
                              t,
                              t(7-1+2,[7/1],
                                t,
                                t(7-2+1,[7/2],
                                  t,
                                  t(7-3+2,[7/3],
                                    t,
                                    t(7-4+1,[7/4],
                                      t,
                                      t(8-1+2,[8/1],
                                        t,
                                        t(8-2+1,[8/2],
                                          t,
                                          t(8-3+2,[8/3],
                                            t,
                                            t(8-4+1,[8/4],
                                              t,
                                              t(9-1+2,[9/1,9/2,9/3,9/4],
                                                t,
                                                t(10-1+2,[10/1,10/2,10/3,10/4],
                                                  t,
                                                  t(11-1+2,[11/1,11/3,11/4],
                                                    t,
                                                    t(11-2+1,[11/2],
                                                     t,
                                                     t(12-1+2,[12/1,12/2,12/3],
                                                       t,
                                                       t(12-2+1,[12/4],
                                                         t,
                                                         t(13-1+1,[13/1,13/2],
                                                           t,
                                                           t(13-2+2,[13/3,13/4],
                                                             t,
                                                             t(14-1+2,[14/1,14/2],
                                                               t,
                                                               t(14-2+1,[14/3,14/4],
                                                                 t,
                                                                 t(15-1+2,[15/1],
                                                                   t,
                                                                   t(15-2+1,[15/2],
                                                                     t,
                                                                     t(15-3+2,[15/3],
                                                                       t,
                                                                       t(15-4+1,[15/4],
                                                                         t,
                                                                         t(16-1+1,[16/1],
                                                                           t,
                                                                           t(16-2+2,[16/2],
                                                                             t,
                                                                             t(16-3+1,[16/3],
                                                                               t,
                                                                               t(16-4+2,[16/4],t,t))))))))))))))))))))))))))))))))))),
            t(1/2,[1-1+1,8/4],t,t(1/3,[1-1+1,4/1],
              t,
              t(1/4,[1-1+1,13/2],
                t,
                t(2/1,[2-1+1,6/3],
                  t,
                  t(2/2,[2-1+1,3/4],
                    t,
                    t(2/3,[2-1+1,5/1],
                      t,
                      t(2/4,[2-1+1,8/2],
                        t,
                        t(3/1,[3-1+1,16/3],
                          t,
                          t(3/2,[3-2+2],
                            t,
                            t(3/3,[3-1+1],
                              t,
                              t(3/4,[3-1+1,2/2],
                                t,
                                t(4/1,[4-1+1,1/3],
                                  t,
                                  t(4/2,[4-1+1,12/4],
                                    t,
                                    t(4/3,[4-2+2,15/1],
                                      t,
                                      t(4/4,[4-1+1,7/2],
                                        t,
                                        t(5/1,[5-1+1,2/3],
                                          t,
                                          t(5/2,[5-1+1],
                                            t,
                                            t(5/3,[5-2+2,10/1],
                                              t,
                                              t(5/4,[5-2+2,12/2],
                                                t,
                                                t(6/1,[6-1+2],
                                                  t,
                                                  t(6/2,[6-1+2,16/4],
                                                    t,
                                                    t(6/3,[6-2+1,2/1],
                                                      t,
                                                      t(6/4,[6-2+1,11/2],
                                                        t,
                                                        t(7/1,[7-1+2,13/3],
                                                          t,
                                                          t(7/2,[7-2+1,4/4],
                                                            t,
                                                            t(7/3,[7-3+2],
                                                              t,
                                                              t(7/4,[7-4+1],
                                                                t,
                                                                t(8/1,[8-1+2,11/3],
                                                                  t,
                                                                  t(8/2,[8-2+1,2/4],
                                                                    t,
                                                                    t(8/3,[8-3+2,12/1],
                                                                      t,
                                                                      t(8/4,[8-4+1,1/2],
                                                                        t,
                                                                        t(9/1,[9-1+2,10/3],
                                                                          t,
                                                                          t(9/2,[9-1+2],
                                                                            t,
                                                                            t(9/3,[9-1+2],
                                                                              t,
                                                                              t(9/4,[9-1+2],
                                                                                t,
                                                                                t(10/1,[10-1+2,5/3],
                                                                                  t,
                                                                                  t(10/2,[10-1+2],
                                                                                    t,
                                                                                    t(10/3,[10-1+2,9/1],
                                                                                      t,
                                                                                      t(10/4,[10-1+2,14/2],
                                                                                        t,
                                                                                        t(11/1,[11-1+2],
                                                                                          t,
                                                                                          t(11/2,[11-2+1,6/4],
                                                                                            t,
                                                                                            t(11/3,[11-1+2,8/1],
                                                                                              t,
                                                                                              t(11/4,[11-1+2],
                                                                                                t,
                                                                                                t(12/1,[12-1+2,8/3],
                                                                                                  t,
                                                                                                  t(12/2,[12-1+2,5/4],
                                                                                                    t,
                                                                                                    t(12/3,[12-1+2,14/1],
                                                                                                      t,
                                                                                                      t(12/4,[12-2+1,4/2],
                                                                                                        t,
                                                                                                        t(13/1,[13-1+1],
                                                                                                          t,
                                                                                                          t(13/2,[13-1+1,1/4],
                                                                                                            t,
                                                                                                            t(13/3,[13-2+2,7/1],
                                                                                                              t,
                                                                                                              t(13/4,[13-2+2],
                                                                                                                t,
                                                                                                                t(14/1,[14-1+2,12/3],
                                                                                                                  t,
                                                                                                                  t(14/2,[14-1+2,10/4],
                                                                                                                    t,
                                                                                                                    t(14/3,[14-2+1],
                                                                                                                      t,
                                                                                                                      t(14/4,[14-2+1,15/2],
                                                                                                                        t,
                                                                                                                        t(15/1,[15-1+2,4/3],
                                                                                                                          t,
                                                                                                                          t(15/2,[15-2+1,14/4],
                                                                                                                            t,
                                                                                                                            t(15/3,[15-3+2],
                                                                                                                              t,
                                                                                                                              t(15/4,[15-4+1],
                                                                                                                                t,
                                                                                                                                t(16/1,[16-1+1],
                                                                                                                                  t,
                                                                                                                                  t(16/2,[16-2+2],
                                                                                                                                    t,
                                                                                                                                    t(16/3,[16-3+1,3/1],
                                                                                                                                      t,
                                                                                                                                      t(16/4,[16-4+2,6/2],t,t))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))),
    balance_assoc(S, BS),
    assoc_stat(S, SStat),
    assoc_stat(BS, BSStat),
    writeln(s(SStat)),
    writeln(bs(BSStat)),
    component_ex([2-1+1], BS, [], C).