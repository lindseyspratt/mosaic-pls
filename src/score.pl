:- module(score,
    [score/1, incremental_score/2, components_score/2, get_components/1, save_score_stream/1,
     test_tiles/0, test_tiles2/0, test_tiles2/2, clear_score/0, create_score/0,
     get_totals/1, add_totals/2, assess_winner/2]).

:- use_module('../proscriptls_sdk/library/data_predicates').

:- use_module(model_basics).
:- use_module(game_model_tiles).
:- use_module(tile_model).
:- use_module(locations).
:- use_module(library).
:- use_module(components).

:- initialization(initdyn).

initdyn :-
    data_mode(Mode),
    data_predicate_dynamics([data_predicates(s, data, [Mode], [components,totals])]).

dummy_reference :-
    dummy_reference,
    data_components(_),
    graph(_,_).

create_score:-
    assert_data(s([],[]), 1).

clear_score :-
    data_default_id(ID),
    clear_components(ID),
    clear_totals(ID).

get_components(Components) :-
    data_components(Components).

clear_components(ID) :-
    clear_data_components(ID).

save_score_stream(Stream) :-
    writeln(save_score),
    save_data_stream(data, Stream),
    writeln(done(save_score)).

add_totals(RoundTurnAfterScore, Score) :-
    previous_turn(RoundTurnAfterScore, RoundTurnForScore),
    data_default_id(ID),
    data_totals(ID, Old),
    get_number_of_players(NumberOfPlayers),
    Limit is NumberOfPlayers * 2,
    append_limit([RoundTurnForScore-Score], Old, Limit, New),
    set_data_totals(ID, New).

append_limit([], Tail, Limit, Result) :-
    limit(Tail, Limit, Result).
append_limit(_, _, 0, []) :-
    !.
append_limit([H|T], Tail, Limit, [H|Result]) :-
    Next is Limit - 1,
    append_limit(T, Tail, Next, Result).

limit([], _, []) :- !.
limit(_, 0, []) :- !.
limit([H|T], Limit, [H|Result]) :-
    Next is Limit - 1,
    limit(T, Next, Result).

get_totals(Totals) :-
    data_totals(Totals).

clear_totals(ID) :-
    clear_data_totals(ID).

% S = [ID1-Scores11, ID2-Scores21, ...]
% for two values of IDX with ScoresNX: previousPlayer(IDX, IDY), setof(V, (member(IDY-V, ScoresNX), V >= 100), Vs).
% If (length(Vs, L), L > 1) then IDY wins.
assess_winner(Scores, Winner) :-
    member(Turn-_, Scores),
    findall(V, (member(Turn-CheckScore, Scores), member(Turn-V, CheckScore), V >= 100), Vs),
    length(Vs, Length),
    Length > 1,
    Winner = Turn.

% score/1 can be run at any time the board is 'stable'.
% incremental_score/2 uses the Components recorded
% by score/1.

score(Scores) :-
    graph,
    gc,
    unique_nodes,
    gc,
    components_ex,
    get_components(Components),
    components_score(Components, Scores).

set_components(Components) :-
    data_default_id(ID),
    set_data_components(ID, Components),
    !.

incremental_score(NewTile, Scores) :-
    data_default_id(ID),
    data_components(ID, OldComponents),
    incremental_score(NewTile, OldComponents, MergedComponents, Scores),
    update_data_components(ID, OldComponents, MergedComponents),
    !.

incremental_score(NewTile, OldComponents, Components, Scores) :-
    graph([NewTile], NewNodes, NewEdges),
    sort_cut(NewNodes, SortedNewNodes),
    components_ex(SortedNewNodes, NewEdges, NewComponents),
    merge_components(NewComponents, OldComponents, Components),
    components_score(Components, Scores).

components_score(Components, Scores) :-
    component_tile_sets(Components, TileSets),
    tile_set_scores(TileSets, Scores).

test_tiles :-
    init_model_basics(2, 4, [1,2,3,4]),
    init_game_model_tiles, % uses info in model_basics.
    update_game_phase,
    increment_turn(1), % the first increment should move the turn from the initial 0 to 1.
    create_score,
    place_tile_on_board(1, 0, 0),
    place_tile_on_board(11, 0, 1),
    place_tile_on_board(2, -1, 0),
    place_tile_on_board(12, -1, 1).

test_tiles2 :-
    init_model_basics(2, 4, [1,2,3,4]),
    init_game_model_tiles, % uses info in model_basics.
    update_game_phase,
    increment_turn(1), % the first increment should move the turn from the initial 0 to 1.
    create_score.

test_tiles2(1, S) :-
    place_tile_on_board(1, 0, 0),
    incremental_score(1, S).

test_tiles2(2, S) :-
    place_tile_on_board(11, 0, 1),
    incremental_score(11, S).

test_tiles2(3, S) :-
    place_tile_on_board(2, -1, 0),
    incremental_score(2, S).

test_tiles2(4, S) :-
    place_tile_on_board(12, -1, 1),
    incremental_score(12, S).

:- dynamic(graph_nodes/1).
:- dynamic(graph_unique_nodes/1).
:- dynamic(graph_edges/1).

lookup_node(Term, ID) :-
    format(atom(ID), '~w', [Term]).

get_node_term(ID, Term) :-
    var(ID)
     -> lookup_node(Term, ID)
    ;
    atom_to_term(ID, Term, _).

unique_nodes :-
    graph_nodes(RawNodes),
    fail_save(sort_cut(RawNodes, Nodes)),
    retractall(graph_unique_nodes(_)),
    asserta(graph_unique_nodes(Nodes)).

/*
Create a graph from the board tiles.
The graph has one or more nodes for each tile.
A tile has one or more regions of same-color adjacent sides.
Each tile region is connected to one or more side nodes.
The side node of one tile is connected by a graph edge to
a side node of an adjacent tile.
*/
graph :-
    fail_save(graph(Nodes, Edges)),
    retractall(graph_nodes(_)),
    asserta(graph_nodes(Nodes)),
    retractall(graph_edges(_)),
    asserta(graph_edges(Edges)).


components_ex :-
    graph_unique_nodes(Nodes),
    graph_edges(Edges),
    length(Nodes, NodeCount),
    length(Edges, EdgeCount),
    writeln(components_ex_counts(NodeCount, EdgeCount)),
    yield,
    components_ex(Nodes, Edges, Components),
    set_components(Components).

graph(Nodes, Edges) :-
    get_board(Board),
    graph(Board, Nodes, Edges).

graph(RawBoard, Nodes, AllEdges) :-
    sort_cut(RawBoard, Board), % order tiles by ascending ID. adjacent_tile_edges/1 relies on this ordering to avoid duplicate edges.
    graph1(Board, Nodes, BaseEdges),
    adjacent_tile_edges(Board, RawAllEdges, BaseEdges),
    sort_cut(RawAllEdges, AllEdges). % remove duplicate edges.

graph1([], [], []).
graph1([H|T], Nodes, Edges) :-
    graph1(H, Nodes, NodesTail, Edges, EdgesTail),
    graph1(T, NodesTail, EdgesTail).

graph1(Tile, Nodes, NodesTail, Edges, EdgesTail) :-
    get_tile_colors(Tile, Colors),
    Colors = [FirstColor|_],
    Regions = [FirstRegion-FirstColor|_],
    graph1(Colors, 0, none, none, FirstColor, FirstRegion, Tile, Regions, GenNodes, Nodes, NodesTail, Edges, EdgesTail),
    bind_regions(Regions),
    generate_nodes(GenNodes).

bind_regions(Regions) :-
    region_components(Regions, Components),
    merge_first_and_last_components(Components, Merged),
    bind_region_components(Merged, 1).

region_components([Region-Color], [[Region-Color]]) :-
    !.
region_components([Region-Color|T], [[Region-Color|ComponentTail]|ComponentsTail]) :-
    region_components(T, Color, ComponentTail, ComponentsTail).

region_components([],_,[],[]).
region_components([Region-Color|T], PreviousColor, Component, Components) :-
    (Color = PreviousColor
      -> Component = [Region-Color|ComponentTail],
         Components = ComponentsTail
    ;
    Component = [],
    Components = [[Region-Color|ComponentTail]|ComponentsTail]
    ),
    region_components(T, Color, ComponentTail, ComponentsTail).

merge_first_and_last_components([First], [First]) :-
    !.
merge_first_and_last_components([First|OtherComponents], Merged) :-
    append(Between, [Last], OtherComponents),
    First = [_-FirstColor|_],
    Last = [_-LastColor|_],
    (FirstColor = LastColor
        -> append(First, Last, Combined),
           Merged = [Combined|Between]
     ;
     Merged = [First|OtherComponents]
    ).

bind_region_components([], _).
bind_region_components([H|T], Counter) :-
    bind_region_component(H, Counter),
    NextCounter is Counter + 1,
    bind_region_components(T, NextCounter).

bind_region_component([], _).
bind_region_component([Counter-_|T], Counter) :-
    bind_region_component(T, Counter).

generate_nodes([]).
generate_nodes([Term - Node|T]) :-
    lookup_node(Term, Node),
    generate_nodes(T).

% If the current color is the same as the previous color then the region is the same.
% Else if the current color is the same as the first color and the current color
% is the last color, then the last region is merged with the first region.
% Else current color starts a new region.
% The regions are unbound variables in this predicate, allowing for the last color region to
% unify with the first color region.
% The regions are later bound to numbers 1 to N by bind_regions/2.

graph1([], _, _, _, _, _, _Tile, [], [], Nodes, Nodes, Edges, Edges).
graph1([H|T], PreviousSideCounter, Region, PreviousColor, FirstColor, FirstRegion,
        Tile,
        Regions,
        [(Tile-NextRegion+H) - Node1, (Tile/SideCounter) - Node2|GenNodesNext],
        [Node1, Node2|NodesInterim], NodesTail,
        [edge(Node1, Node2)|EdgesInterim], EdgesTail) :-
    SideCounter is PreviousSideCounter + 1,
    (H = PreviousColor
      -> NextRegion = Region,
         Regions = RegionsTail
    ;
     H = FirstColor,
     T = []
      -> NextRegion = FirstRegion,
         Regions = RegionsTail
    ;
     Regions = [NextRegion-H|RegionsTail] % NextRegionCounter is an unbound variable that *may* identify a new region.
    ),
    graph1(T, SideCounter, NextRegion, H, FirstColor, FirstRegion, Tile, RegionsTail,
            GenNodesNext, NodesInterim, NodesTail, EdgesInterim, EdgesTail).

adjacent_tile_edges([], Edges, Edges).
adjacent_tile_edges([Tile|OtherTiles], Edges, EdgesTail) :-
    ateDX([-1,0, 1], Tile, Edges, EdgesInterim),
    adjacent_tile_edges(OtherTiles, EdgesInterim, EdgesTail).

ateDX([], _, Edges, Edges).
ateDX([DX|DXTail], Tile, Edges, EdgesTail) :-
    ateDY([-1,0, 1], DX, Tile, Edges, EdgesInterim),
    ateDX(DXTail, Tile, EdgesInterim, EdgesTail).

ateDY([], _DX, _Tile, Edges, Edges).
ateDY([DY|T], DX, Tile, Edges, EdgesTail) :-
    ateDXDY(DX, DY, Tile, Edges, EdgesInterim),
    ateDY(T, DX, Tile, EdgesInterim, EdgesTail).

% There appears to be a ProscriptLS bug in this clause where the wrong variable is used in the
%   Edges = [edge(Tile/TilePositionIndex, OtherTile/OtherTilePositionIndex)|EdgesTail]
% instead attempting the equivalent of
%   TileColor = [edge(Tile/TilePositionIndex, OtherTile/OtherTilePositionIndex)|EdgesTail]
% which fails.
% Rewriting the single clause as several predicates none of which use (-> ; ...) cliche
% works.
%
%ateDXDY(DX, DY, Tile, Edges, EdgesTail) :-
%    (DX=0;DY=0)
%      -> (get_neighboring_tile(Tile, DX, DY, TilePositionIndex, OtherTile, OtherTilePositionIndex)
%           -> get_tile_colors(Tile, Colors),
%              nth1(TilePositionIndex, Colors, TileColor),
%              get_tile_colors(OtherTile, OtherColors),
%              (Tile < OtherTile, % add edge between tile 1 and tile 2 only once - edges are bidirectional.
%               nth1(OtherTilePositionIndex, OtherColors, TileColor)
%               -> Edges = [edge(Tile/TilePositionIndex, OtherTile/OtherTilePositionIndex)|EdgesTail]
%              ;
%               Edges = EdgesTail
%              )
%        ;
%         Edges = EdgesTail
%        )
%    ;
%    Edges = EdgesTail.

ateDXDY(DX, DY, Tile, Edges, EdgesTail) :-
    (DX = 0;DY=0),
    (DX \= 0; DY \= 0),
    !,
    ateDXDY1(DX, DY, Tile, Edges, EdgesTail).
ateDXDY(_DX, _DY, _Tile, Edges, Edges).

ateDXDY1(DX, DY, Tile, Edges, EdgesTail) :-
    get_neighboring_tile(Tile, DX, DY, TilePositionIndex, OtherTile, OtherTilePositionIndex),
    !,
    ateDXDY2(Tile, Edges, EdgesTail, TilePositionIndex, OtherTile, OtherTilePositionIndex).
ateDXDY1(_DX, _DY, _Tile, Edges, Edges).

% Ensure edges are always LowTile -> HighTile, so that sorting can eliminate duplicates.
ateDXDY2(Tile, Edges, EdgesTail, TilePositionIndex, OtherTile, OtherTilePositionIndex) :-
    Tile < OtherTile,
    !,
    ateDXDY3(Tile, Edges, EdgesTail, TilePositionIndex, OtherTile, OtherTilePositionIndex).
ateDXDY2(Tile, Edges, EdgesTail, TilePositionIndex, OtherTile, OtherTilePositionIndex) :-
    Tile > OtherTile,
    ateDXDY3(OtherTile, Edges, EdgesTail, OtherTilePositionIndex, Tile, TilePositionIndex).

ateDXDY3(Tile, Edges, EdgesTail, TilePositionIndex, OtherTile, OtherTilePositionIndex) :-
    get_tile_colors(Tile, Colors),
    nth1(TilePositionIndex, Colors, TileColor),
    get_tile_colors(OtherTile, OtherColors),
    nth1(OtherTilePositionIndex, OtherColors, TileColor),
    lookup_node(Tile/TilePositionIndex, Node1),
    lookup_node(OtherTile/OtherTilePositionIndex, Node2),
    !,
    Edges = [edge(Node1, Node2)|EdgesTail].
ateDXDY3(_Tile, Edges, Edges, _TilePositionIndex, _OtherTile, _OtherTilePositionIndex).

get_neighboring_tile(Tile, DX, DY, TilePositionIndex, OtherTile, OtherTilePositionIndex) :-
    %placed_position_offset(DX, DY, TilePositionOffset),
    edge_neighbor_offset(TilePositionOffset, DX>DY),
    TilePositionIndex is 1 + TilePositionOffset,
    OtherTilePositionIndex is 1 + ((TilePositionOffset + 2) mod 4),
    get_tile_grid_x(Tile, TX),
    get_tile_grid_y(Tile, TY),
    OX is TX + DX,
    OY is TY + DY,
    get_board_tile_by_grid(OX>OY, OtherTile).


component_tile_sets([], []).
component_tile_sets([H|T], [Color-TileSet|OtherTileSets]) :-
    component_tile_set(H, Color, RawTileSet),
    sort_cut(RawTileSet, TileSet), % remove duplicate Tile references.
    component_tile_sets(T, OtherTileSets).

% component_tile_set(H, TileSet)
component_tile_set([], _, []).
component_tile_set([H|T], Color, [Tile|OtherTiles]) :-
    node_tile(H, Tile, Color),
    component_tile_set(T, Color, OtherTiles).

% Tile-NextRegion, Tile/SideCounter
node_tile(NodeID, Tile, Color) :-
    get_node_term(NodeID, Term),
    node_tile1(Term, Tile, Color).

node_tile1(Tile-_Region + Color, Tile, Color) :- !.
node_tile1(Tile/_SideCounter, Tile, _).

% tile_set_scores(TileSets, Scores)
tile_set_scores(TileSets, Scores) :-
    tile_set_scores(TileSets, [], Scores).

tile_set_scores([], Scores, Scores).
tile_set_scores([H|T], ScoresIn, ScoresOut) :-
    tile_set_score(H, ScoresIn, ScoresNext),
    tile_set_scores(T, ScoresNext, ScoresOut).

tile_set_score(Color-TileSet, ScoresIn, ScoresOut) :-
    length(TileSet, Length),
    (Length = 1
      -> Value = 0
    ;
     Value is Length * Length
    ),
    (select(Color-ScoreIn, ScoresIn, Color-ScoreOut, ScoresOut)
      -> ScoreOut is ScoreIn + Value
    ;
     ScoresOut = [Color-Value|ScoresIn]
    ).
