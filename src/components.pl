:- module(components, [components_dfs/3, components_ex/3, merge_components/3]).

:- use_module(library).

dummy_reference :-
    dummy_reference,
    connected(_,_,_).

% merge_components([[a,b], [c,d], [e,f]], [[b,d,g]], M).

merge_components(Components1, Components2, MergedComponents) :-
    sort_lists(Components1, SortedC1),
    sort_lists(Components2, SortedC2),
    merge_components1(SortedC1, SortedC2, MergedComponents).

merge_components1([], M, M).
merge_components1([H|T], Others, MOut) :-
    mc2(Others, H, MOthers, UnmergedOthers),
    (MOthers = []
      -> NewM = H
    ;
     mc_union_lists(MOthers, NewM) % H is incorporated to component(s) in MOthers
    ),
    merge_components1(T, [NewM|UnmergedOthers], MOut).

% mc2(OldComponents, NewComponent, Merges, UnmergedOld)
%
% Merge each component of OldComponents with NewComponent
% if they have a non-empty intersection.
% 'Merges' is a list of all of these successful merges.
% 'UnmergedOld' is a list of all of the OldComponents that
% were not merged with NewComponent.

mc2([], _, [], []).
mc2([HOld|TOld], NewComponent, Merges, UnmergedOld) :-
    (mc3(HOld, NewComponent, MergeOld)
      -> Merges = [MergeOld|MergesTail],
         UnmergedOld = UnmergedOldTail
    ;
    Merges = MergesTail,
    UnmergedOld = [HOld|UnmergedOldTail]
    ),
    mc2(TOld, NewComponent, MergesTail, UnmergedOldTail).

% mc3(+OldComponent, +NewComponent, ?MergedCompent)
%
% Merge OldComponent and NewComponent if there is at
% least one item in their intersection.
% Otherwise fail.
% OldComponent, NewComponent, and MergedComponent are term-sorted
% lists of distinct terms.

mc3([], _, _) :- !, fail.
mc3(_, [], _) :- !, fail.
mc3([H|T], [H2|T2], M) :-
    H @< H2
      -> M = [H|MT],
         mc3(T, [H2|T2], MT)
    ;
     H @> H2
      -> M = [H2|MT],
         mc3([H|T], T2, MT)
    ;
     H = H2
      -> M = [H|MT],
         mc_union(T, T2, MT).

mc_union_lists(Lists, Union) :-
    mc_union_lists(Lists, [], Union).

mc_union_lists([], U, U).
mc_union_lists([H|T], UI, UO) :-
    mc_union(H, UI, UN),
    mc_union_lists(T, UN, UO).

mc_union([], M, M) :- !.
mc_union(M, [], M) :- !.
mc_union([H|T], [H2|T2], M) :-
    (H @< H2
      -> M = [H|MT],
         N = T,
         N2 = [H2|T2]
    ;
     H @> H2
      -> M = [H2|MT],
         N = [H|T],
         N2 = T2
    ;
     H = H2
      -> M = [H|MT],
         N = T,
         N2 = T2
    ),
    mc_union(N, N2, MT).

sort_lists(Lists, SortedLists) :-
    sort_lists1(Lists, BaseSortedLists),
    sort_cut(BaseSortedLists, SortedLists).

sort_lists1([], []).
sort_lists1([H|T], [HS|TS]) :-
    sort_cut(H, HS),
    sort_lists1(T, TS).

% components([a,b,c,d], [edge(a,b), edge(a,c)], C).
components_dfs(Nodes, Edges, Components) :-
    components_dfs(Nodes, Edges, [], Components).

components_dfs([], _Edges, _Seen, []).
components_dfs([H|T], Edges, Seen, Components) :-
    (member(H, Seen)
      -> Components = ComponentsTail,
         SeenNext = Seen
    ;
    component_dfs(H, Edges, [], Component),
    Components = [Component|ComponentsTail],
    append(Seen, Component, SeenNext)
    ),
    components_dfs(T, Edges, SeenNext, ComponentsTail).

component_dfs(Node, Edges, Comp, CompOut) :-
    member(Node, Comp)
      -> CompOut = Comp
    ;
    NextComp = [Node|Comp],
    (setof(Other, connected(Node, Other, Edges), Others)
      -> component_dfs1(Others, Edges, NextComp, CompOut)
    ;
    NextComp = CompOut
    ).

component_dfs1([], _Edges, Comp, Comp).
component_dfs1([H|T], Edges, CompIn, CompOut) :-
    component_dfs(H, Edges, CompIn, CompNext),
    component_dfs1(T, Edges, CompNext, CompOut).

connected(Node, Other, Edges) :-
    member(edge(Node, Other), Edges).
connected(Node, Other, Edges) :-
    member(edge(Other, Node), Edges).

% for Ns, find all direct extensions Ks.
% difference Ks and Seen to get NewKs.
% Seen2 = Seen+NewKs
% Ns2 = NewKs
% for Ns2, find Ks2, diff Ks2 and Seen2 to get NewKs2.
% repeat.

components_ex(Nodes, Edges, Components) :-
    sgraph(Edges, RSG),
    balance_tree(RSG, SG),
    !,
%    writeln(done(sgraph(SG))),
%    yield,
    components_ex(Nodes, SG, [], Components).

%components_ex(Nodes, Edges, Components) :-
%    components_ex(Nodes, Edges, [], Components).

components_ex([], _Edges, _Seen, []).
components_ex([H|T], Edges, Seen, Components) :-
    component_ex(H, Edges, Seen, SeenNext, Components, ComponentsTail),
    components_ex(T, Edges, SeenNext, ComponentsTail).

component_ex(H, _Edges, Seen, Seen, Components, Components) :-
    member(H, Seen),
    !.
component_ex(H, Edges, Seen, SeenNext, Components, ComponentsTail) :-
%    writeln(start(component_ex([H]))),
%    yield,
    component_ex([H], Edges, [], Component),
%    writeln(done(component_ex(Component))),
%    yield,
    Components = [Component|ComponentsTail],
    append(Seen, Component, SeenNext).

component_ex(Nodes, Edges, ComponentIn, ComponentOut) :-
    append(Nodes, ComponentIn, ComponentNext),
    extend_sgraph(Nodes, Edges, ExtendedNodes),
    difference(ExtendedNodes, ComponentNext, NewExtendedNodes),
%    writeln(step(component_ex(Nodes, ComponentIn, ComponentNext, NewExtendedNodes))),
%    yield,
    component_ex1(NewExtendedNodes, Edges, ComponentNext, ComponentOut).

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

component_ex1([], _Edges, ComponentOut, ComponentOut) :-
    !.
component_ex1(NewExtendedNodes, Edges, ComponentNext, ComponentOut) :-
    component_ex(NewExtendedNodes, Edges, ComponentNext, ComponentOut).


extend(Nodes, Edges, ExtendedNodes) :-
    (setof(Other, Node^(member(Node, Nodes), connected(Node, Other, Edges)), ExtendedNodes)
      -> true
    ;
    ExtendedNodes = []
    ).

sgraph(Edges, SG) :-
    sgraph(Edges, t, SG).

sgraph([], SG, SG).
sgraph([edge(A,B)|T], SGIn, SGOut) :-
    add_assoc(A, B, SGIn, SGNext1),
    add_assoc(B, A, SGNext1, SGNext2),
    sgraph(T, SGNext2, SGOut).

add_assoc(A, B, SGIn, SGOut) :-
    (get_assoc(A, SGIn, ATs)
      -> sort([B|ATs], SATs)
    ;
     SATs = [B]
    ),
    put_assoc(A, SGIn, SATs, SGOut).

extend_sgraph(Nodes, SG, Targets) :-
    extend_sgraph(Nodes, SG, [], Targets).

extend_sgraph([], _SG, Targets, Targets).
extend_sgraph([H|T], SG, TargetsIn, TargetsOut) :-
    extend_sgraph1(H, SG, TargetsIn, TargetsNext),
    extend_sgraph(T, SG, TargetsNext, TargetsOut).

extend_sgraph1(Node, SG, TI, TO) :-
    get_assoc(Node, SG, Targets),
    !,
    append(Targets, TI, TO).
extend_sgraph1(_Node, _SG, T, T).


difference(Minuend, Subtrahend, Difference) :-
    setof(X, (member(X, Minuend), \+member(X, Subtrahend)), Difference)
      -> true
    ;
    Difference = [].

% ordered_difference([a,c,e], [b,c,d], R).

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

balance_tree(T,B) :-        % This balances the tree +T giving -B
    assoc_to_list(T,L),     % If you need balanced trees, of course,
    assoc_to_list(B,L).     % there are better ways than this

assoc_stat(Tree, Info) :-
    assoc_stat(Tree, 0, i, Info).

assoc_stat(t, Depth, InfoIn, InfoOut) :-
    update_info(Depth, InfoIn, InfoOut).
assoc_stat(t(_K, _V, L, R), Depth, InfoIn, InfoOut) :-
    X is Depth + 1,
    assoc_stat(L, X, InfoIn, InfoL),
    assoc_stat(R, X, InfoL, InfoOut).

update_info(Depth, i, i(1, Depth, 1, 1, Depth)) :- !.
update_info(Depth, i(Count, Total, Min, Max, _Avg), i(CountOut, TotalOut, MinOut, MaxOut, AvgOut)) :-
    MinOut is min(Min, Depth),
    MaxOut is max(Max, Depth),
    CountOut is Count + 1,
    TotalOut is Total + Depth,
    AvgOut is Total / Count.

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
    balance_tree(S, BS),
    assoc_stat(S, SStat),
    assoc_stat(BS, BSStat),
    writeln(s(SStat)),
    writeln(bs(BSStat)),
    component_ex([2-1+1], BS, [], C).