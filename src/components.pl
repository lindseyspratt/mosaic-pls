:- module(components, [components_dfs/3, components_ex/3]).

dummy_reference :-
    dummy_reference,
    connected(_,_,_).

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