:- module(components, [components_ex/3, merge_components/3]).

:- use_module(library).

dummy_reference :-
    dummy_reference,
    sgraph(_,_),
    extend_sgraph(_,_,_).

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

% for Ns, find all direct extensions Ks.
% difference Ks and Seen to get NewKs.
% Seen2 = Seen+NewKs
% Ns2 = NewKs
% for Ns2, find Ks2, diff Ks2 and Seen2 to get NewKs2.
% repeat.

components_ex(Nodes, Edges, Components) :-
    fail_save(sgraph(Edges, RSG)),
    fail_save(balance_assoc(RSG, SG)),
    !,
%    writeln(done(sgraph(SG))),
%    yield,
    components_ex(Nodes, SG, [], Components).

components_ex([], _Edges, _Seen, []).
components_ex([H|T], Edges, Seen, Components) :-
    component_ex(H, Edges, Seen, SeenNext, Components, ComponentsTail),
    !,
    gc,
    components_ex(T, Edges, SeenNext, ComponentsTail).

component_ex(H, _Edges, Seen, Seen, Components, Components) :-
    member(H, Seen),
    !.
component_ex(H, Edges, Seen, SeenNext, Components, ComponentsTail) :-
    component_ex([H], Edges, [], Component),
    Components = [Component|ComponentsTail],
    append(Seen, Component, SeenNext).

component_ex(Nodes, Edges, ComponentIn, ComponentOut) :-
    append(Nodes, ComponentIn, ComponentNext),
    extend_sgraph(Nodes, Edges, ExtendedNodes),
    difference(ExtendedNodes, ComponentNext, NewExtendedNodes),
    component_ex1(NewExtendedNodes, Edges, ComponentNext, ComponentOut).

component_ex1([], _Edges, ComponentOut, ComponentOut) :-
    !.
component_ex1(NewExtendedNodes, Edges, ComponentNext, ComponentOut) :-
    component_ex(NewExtendedNodes, Edges, ComponentNext, ComponentOut).

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
