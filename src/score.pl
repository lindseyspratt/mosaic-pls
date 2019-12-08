:- module(score,
    [score/1, incremental_score/2, components_score/2, get_components/1, save_score_stream/1,
     test_tiles/0, test_tiles2/0, test_tiles2/2, clear_score/0, create_score/0]).

:- use_module('../proscriptls_sdk/library/data_predicates').

:- use_module(model_basics).
:- use_module(game_model_tiles).
:- use_module(tile_model).
:- use_module(locations).
:- use_module(library).
:- use_module(components).

:- initialization(initdyn).

initdyn :-
    data_predicate_dynamics([data_predicates(s, data,[components])]).

dummy_reference :-
    dummy_reference,
    data_components(_).

create_score:-
    assert_data(s([]), 1).

clear_score :-
    retract_data(data, 1).

get_components(Components) :-
    data_components(Components).

save_score_stream(Stream) :-
    writeln(save_score),
    save_data_stream(data, Stream),
    writeln(done(save_score)).

% score/1 can be run at any time the board is 'stable'.
% incremental_score/2 uses the Components recorded
% by score/1.

score(Scores) :-
    wam_duration(Start),
    graph(RawNodes, Edges),
    sort(RawNodes, Nodes),
    wam_duration(Mark1),
    components_ex(Nodes, Edges, Components),
    set_components(Components),
    wam_duration(Mark2),
    components_score(Components, Scores),
    wam_duration(End),
    display_spans([Start, Mark1, Mark2, End], score).

set_components(Components) :-
    data_default_id(ID),
    retractall(data_components(ID, _)),
    asserta(data_components(ID, Components)).

incremental_score(NewTile, Scores) :-
    data_default_id(ID),
    retract(data_components(ID, OldComponents)),
    incremental_score(NewTile, OldComponents, MergedComponents, Scores),
    asserta(data_components(ID, MergedComponents)).

incremental_score(NewTile, OldComponents, Components, Scores) :-
    graph([NewTile], NewNodes, NewEdges),
    sort(NewNodes, SortedNewNodes),
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

/*
Create a graph from the board tiles.
The graph has one or more nodes for each tile.
A tile has one or more regions of same-color adjacent sides.
Each tile region is connected to one or more side nodes.
The side node of one tile is connected by a graph edge to
a side node of an adjacent tile.
*/
graph(Nodes, Edges) :-
    get_board(Board),
    graph(Board, Nodes, Edges).

graph(RawBoard, Nodes, AllEdges) :-
    sort(RawBoard, Board), % order tiles by ascending ID. adjacent_tile_edges/1 relies on this ordering to avoid duplicate edges.
    graph1(Board, Nodes, BaseEdges),
    adjacent_tile_edges(Board, RawAllEdges, BaseEdges),
    sort(RawAllEdges, AllEdges). % remove duplicate edges.

graph1([], [], []).
graph1([H|T], Nodes, Edges) :-
    graph1(H, Nodes, NodesTail, Edges, EdgesTail),
    graph1(T, NodesTail, EdgesTail).

graph1(Tile, Nodes, NodesTail, Edges, EdgesTail) :-
    get_tile_colors(Tile, Colors),
    Colors = [FirstColor|_],
    Regions = [FirstRegion-FirstColor|_],
    graph1(Colors, 0, none, none, FirstColor, FirstRegion, Tile, Regions, Nodes, NodesTail, Edges, EdgesTail),
    bind_regions(Regions, 1).

bind_regions([], _).
bind_regions([Counter-_|T], Counter) :-
    NextCounter is Counter + 1,
    bind_regions(T, NextCounter).

% If the current color is the same as the previous color then the region is the same.
% Else if the current color is the same as the first color and the current color
% is the last color, then the last region is merged with the first region.
% Else current color starts a new region.
% The regions are unbound variables in this predicate, allowing for the last color region to
% unify with the first color region.
% The regions are later bound to numbers 1 to N by bind_regions/2.

graph1([], _, _, _, _, _, _Tile, [], Nodes, Nodes, Edges, Edges).
graph1([H|T], PreviousSideCounter, Region, PreviousColor, FirstColor, FirstRegion,
        Tile,
        Regions,
        [Tile-NextRegion+H, Tile/SideCounter|NodesInterim], NodesTail,
        [edge(Tile-NextRegion+H, Tile/SideCounter)|EdgesInterim], EdgesTail) :-
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
            NodesInterim, NodesTail, EdgesInterim, EdgesTail).

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
    !,
    Edges = [edge(Tile/TilePositionIndex, OtherTile/OtherTilePositionIndex)|EdgesTail].
ateDXDY3(_Tile, Edges, Edges, _TilePositionIndex, _OtherTile, _OtherTilePositionIndex).

get_neighboring_tile(Tile, DX, DY, TilePositionIndex, OtherTile, OtherTilePositionIndex) :-
    placed_position_offset(DX, DY, TilePositionOffset),
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
    sort(RawTileSet, TileSet), % remove duplicate Tile references.
    component_tile_sets(T, OtherTileSets).

% component_tile_set(H, TileSet)
component_tile_set([], _, []).
component_tile_set([H|T], Color, [Tile|OtherTiles]) :-
    node_tile(H, Tile, Color),
    component_tile_set(T, Color, OtherTiles).

% Tile-NextRegion, Tile/SideCounter
node_tile(Tile-_Region + Color, Tile, Color).
node_tile(Tile/_SideCounter, Tile, _).

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

/*

function graphAndScore(gameTiles) {
    var g = new Graph();
    g.edgeFactory.template.style.directed = false;

    var tileNames = {};
//	console.log("new graph");

	for(var tileKey in gameTiles) {
		var tile = gameTiles[tileKey];

		var tileIsCross = true;
		for(var ofst = 0;ofst < tile.colors.length;ofst++) {
			var nextOfst = (ofst + 1) % 4;
			if(tile.colors[ofst] === tile.colors[nextOfst]) {
				tileIsCross = false;
			}
		}

		for(var ofst = 0;ofst < tile.colors.length;ofst++) {
		//var ofst = 0;
			var tileName = "t" + tile.id
			+ (tileIsCross ? (":" + ofst) : "")
			+ "-" + tile.colors[ofst];

			tileNames[tileName] = tile.colors[ofst];

			var edgeName = "e" + tile.id + ":" + ofst;
			g.addEdge(tileName, edgeName);
		}

		// check orthogonal neighboring tiles for
		// connections.

		for(var ofst = 0;ofst < tile.colors.length;ofst++) {
	        for (var dy = -1; dy <= 1; dy++) {
	            for (var dx = -1; dx <= 1; dx++) {
	                var tx = tile.boardTileX + dx;
	                var ty = tile.boardTileY + dy;
	                var key = boardHashKeyCoords(tx, ty);
	                if (ifdefor(game.board.hash[key])) {
	                    if (dx === 0 || dy === 0) {
	                    	var otherTile = game.board.hash[key];
	                    	// orthogonal location
	                        var placedPositionOffset = (dy < 0) ? 0
                                    : ((dy > 0) ? 2
                                                : ((dx < 0) ? 3 : 1));
	                        var otherPositionOffset = (placedPositionOffset + 2) % 4;

	            			var edgeName = "e" + tile.id + ":" + placedPositionOffset;
	            			var otherEdgeName = "e" + otherTile.id + ":" + otherPositionOffset;

	            			// avoid adding the same edge twice with the
	            			// ends swapped - the edges are meant to be undirected.
	            			if(tile.id < otherTile.id) {
	            				g.addEdge(edgeName, otherEdgeName);
	            			}
	                    }
	                }
	            }
	        }
		}
	}

//	console.log("graph built");
//	console.log(g)
	return score(g, tileNames);
}

function score(g, tiles) {

    / * Step 1: initializing empty path matrix (second dimension is implicit) * /
    var adjacency = {};

//    console.log(tiles);

    / * construct path matrix, initialize with 0 and 1 * /
    for(j in g.nodes) {
        adjacency[j] = {};
        for(i in g.nodes) {
            adjacency[j][i] = (j === i) ? 1 : 0;
        }
    }

    / * initialize path connections - undirected * /
    for(e in g.edges) {
    	var src = g.edges[e].source.id;
    	var tgt = g.edges[e].target.id;
        adjacency[src][tgt] = 1;
        adjacency[tgt][src] = 1;
    }

    / * Step 2: extend connections.
     * Find the fixed point: run the loop until no more new path extensions occur.
     * /
    var changed = true;
    while(changed) {
    	changed = false;
    	for(k in g.nodes){
    		for(i in g.nodes) {
    			for(j in g.nodes) {
    				if(adjacency[i][j] === 0 && adjacency[i][k] == 1 && adjacency[k][j] == 1) {
    					adjacency[i][j] = 1;
    					adjacency[j][i] = 1;
    					changed = true;
    				}
    			}
    		}
    	}
    }

//    console.log(adjacency);
    / *
     * gather tiles into connected components.
     * /
    var componentMap = {};
    var components = [];


    for(tile in tiles) {
//    	console.log(tile);

    	if(componentMap[tile]) {
//    		console.log("skip - already in map");
    		continue;
    	}


    	var row = adjacency[tile];
//    	console.log("row for " + tile + " = " + row);
    	var component = {};
    	// tN:K-J
    	var adjustedTile = tile.replace(/:\d/, "");
    	component[adjustedTile] = 1;
    	for(node in row) {
    		var adjacent = row[node];
    		var tileNodeColor = tiles[node];
//    		console.log("node " + node);
//    		console.log("adjacent " + adjacent);
//    		console.log("tileNodeColor " + tileNodeColor);
    		if(tileNodeColor != undefined && adjacent) {
    	    	if(componentMap[node]) {
    	    		console.log("unexpected component: " + node);
    	    	}
    	    	var adjustedJ = node.replace(/:\d/, "");
    			component[adjustedJ] = 1;
    	    	componentMap[node] = component;
    		}
    	}
//    	console.log(component);
    	componentMap[tile] = component;
    	components.push(component);
    }

//    console.log("map: " + JSON.stringify(componentMap));

    var score = [0,0];

    for(var ofst = 0;ofst < components.length;ofst++) {
    	var component = components[ofst];
    	var tiles = Object.keys(component);
//    	console.log("a: " + tiles);
    	var size = tiles.length;
//    	console.log(size);
    	if(size > 1) {
    		var tile = tiles[0];
    		var splits = tile.split("-");

    		score[splits[1]] = score[splits[1]] + size * size;
    	}
    }


    return score;
}

*/

