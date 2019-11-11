:- module(components, [components_dfs/3, components_ex/3, merge_components/3]).

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
    sort(BaseSortedLists, SortedLists).

sort_lists1([], []).
sort_lists1([H|T], [HS|TS]) :-
    sort(H, HS),
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
    components_ex(Nodes, Edges, [], Components).

components_ex([], _Edges, _Seen, []).
components_ex([H|T], Edges, Seen, Components) :-
    (member(H, Seen)
      -> Components = ComponentsTail,
         SeenNext = Seen
    ;
    component_ex([H], Edges, [], Component),
    Components = [Component|ComponentsTail],
    append(Seen, Component, SeenNext)
    ),
    components_ex(T, Edges, SeenNext, ComponentsTail).

component_ex(Nodes, Edges, ComponentIn, ComponentOut) :-
    append(Nodes, ComponentIn, RawComponentNext),
    sort(RawComponentNext, ComponentNext),
    extend(Nodes, Edges, ExtendedNodes),
    sort(ExtendedNodes, SortedExtendedNodes),
    ordered_difference(SortedExtendedNodes, ComponentIn, NewExtendedNodes),
    (NewExtendedNodes = []
      -> ComponentNext = ComponentOut
    ;
    component_ex(NewExtendedNodes, Edges, ComponentNext, ComponentOut)
    ).

extend(Nodes, Edges, ExtendedNodes) :-
    (setof(Other, Node^(member(Node, Nodes), connected(Node, Other, Edges)), ExtendedNodes)
      -> true
    ;
    ExtendedNodes = []
    ).

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