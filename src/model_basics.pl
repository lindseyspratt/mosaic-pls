/**
 * gameModelBasics function creates an object that defines basic parameters of a game.
 * These attributes are accessed using these functions:
 * get_hand_color_ids : color ID sequences for all hands 0 through numberOfPlayers - 1.
 *
 * get_number_of_players : number of players in the game (2, 3, or 4).
 *
 * get_triangles_per_tile : number of triangles per tile, either 4 for square tiles or 6 for hexagonal tiles.
 * ---
 * The basics object also defines a number of utility functions:
 * rotate_right : rotate a color sequence 'right' moving the 0'th element to the end (K'th) position, shifting
 * all other items down one position.
 *
 * rotate_left : rotate a color sequence 'left' moving the last (K'th) element to the beginning (0'th) position,
 * shifting all other items up one position.
 *
 * board_hash_key_coords : convert a pair of coordinates to a unique string used as a lookup key. E.g. (2,3)
 * becomes 'x2y3'.
 *
 * tile_colors_match_constraint_colors : returns a true if there are no mismatches between a color constraint
 * sequence and a tile color sequence. The mismatch count of position offsets in a color constraint sequence
 * and a tile color sequence is incremented for a given position offset in the two sequences where
 * the color constraint value is >= 0 and the tile color ID is not equal to the color constraint ID.
 * In this definition any color constraint value < 0 matches any tile color ID.
 *
 * edge_neighbor_offset : maps an 'edge' identifer (0 to 3 for squares) to a dx>dy pair to find the neighbor tile
 * position across that edge from a given tile position. From a tile at grid position (1,1), the neighbor across
 * edge 0 has (dx,dy) of (0,-1). This gives the edge 0 neighbor grid position of (1, 0).
 *
 * values : The 'values' function creates a displayable-on-the-console object that is the labeled values
 * returned by the parameter-defining functions of the gameModelBasics object - get_hand_color_ids,
 * get_number_of_players, and get_triangles_per_tile.
 *
 * @param {Object} spec - specification of parameters for the basics of the game model.
 * @param {number} spec.numberOfPlayers
 * @param {number} spec.trianglesPerTile
 */

:- module(model_basics, [init_model_basics/3, save_model_basics/0, load_model_basics/0,
    save_model_basics_stream/1, retract_model_basics/0,
    get_hand_color_ids/1, get_number_of_players/1, get_triangles_per_tile/1,
    rotate_right/2, rotate_left/2, board_hash_key_coords/3,
    tile_colors_match_constraint_colors/2, tile_colors_mismatch_constraint_colors/4,
    edge_neighbor_offset/2, display_spans/2, values/1,
    undoable_update/2, undo_update/2]).

:- use_module('../proscriptls_sdk/library/data_predicates').
:- use_module(library).

:- meta_predicate((
    undoable_update((:), (:)),
    undo_update((:), (:))
    )).

:- initialization(initdyn).

initdyn :-
    data_predicate_dynamics([data_predicates(gmb, data,[numberOfPlayers, trianglesPerTile, abstractColors, handColorIDSequences])]).

% init(2, 4, [a,b,c,d])
init_model_basics(NOP, TPT, AC) :-
    assert_data(gmb(NOP, TPT, AC, []), 1),
	build_sequences(NOP, HCIS),
    assert_data(gmb(NOP, TPT, AC, HCIS), 1).

save_model_basics_stream(Stream) :-
    writeln(save_model_basics),
    save_data_stream(data, Stream),
    writeln(done(save_model_basics)).

retract_model_basics :-
    retract_all_data(data).

save_model_basics :-
    writeln(save_model_basics),
    save_data(data, local_storage('mosaic')),
    writeln(done(save_model_basics)).

load_model_basics :-
    load_data(data, local_storage('mosaic')).

/**
 * build_base_sequences constructs an array of color sequences that defines the basic collection of
 * tiles used to construct a hand of tiles. This basic collection contains various one and two color
 * tiles using the 'Major' and 'Minor' input colors. The sequences always contain at least as many
 * Major elements at Minor elements.
 * @param {number} Major - color ID
 * @param {number} Minor - color ID
 * @returns {number[][]} baseSequence
 */
build_base_sequences(Major,Minor,S) :-
	data_trianglesPerTile(TPT),
	(TPT = 4
	  -> S = [[Major,Major,Major,Major], [Major,Major,Major,Major], [Minor,Major,Major,Major],
	        [Minor,Major,Major,Major], [Minor,Minor,Major,Major], [Major,Major,Minor,Minor],
	        [Minor,Major,Minor,Major], [Major,Minor,Major,Minor]]
	;
	 TPT = 6
	  -> S = [[Major,Major,Major,Major,Major,Major], [Major,Major,Major,Major,Major,Major],
	        [Minor,Major,Major,Major,Major,Major], [Minor,Major,Major,Major,Major,Major],
            [Minor,Minor,Major,Major,Major,Major], [Minor,Minor,Major,Major,Major,Major],
            [Minor,Minor,Minor,Major,Major,Major], [Major,Major,Major,Minor,Minor,Minor],
            [Minor,Major,Minor,Major,Major,Major], [Minor,Major,Minor,Major,Major,Major],
            [Minor,Major,Minor,Major,Minor,Major], [Major,Minor,Major,Minor,Major,Minor]]
	).

build_sequences(N, Ps) :-
    length(ACs, N),
    append(ACs, _, DACs),
    data_abstractColors(DACs),
    hands(ACs, [], Ps).

hands([], _, []).
hands([H|T], Previous, [P|Ps]) :-
    append(Previous, T, Others),
    hands1(Others, H, P),
    hands(T, [H|Previous], Ps).

hands1([], _Main, []).
hands1([H|T], Main, P) :-
    build_base_sequences(Main, H, Pair),
    append(Pair, PTail, P),
    hands1(T, Main, PTail).

/**
 * get_hand_color_ids returns an array of color ID sequences for hands 0 through numberOfPlayers-1,
 * where color ID is an integer mapped to a color and hand
 * with offset K (0 to number of players - 1) is for player K.
 * Each color ID sequence is used to define a tile by specifying the colors of the
 * triangles of that tile.
 * @type {function(): number[][][]}
 */
get_hand_color_ids(IDs) :-
    data_handColorIDSequences(IDs).

/**
 * get_number_of_players returns number of players in the game (2, 3, or 4).
 * @type {function(): number}
 */
get_number_of_players(NOP) :-
    data_numberOfPlayers(NOP).

/**
 * get_triangles_per_tile returns number of triangles per tile,
 * either 4 for square tiles or 6 for hexagonal tiles.
 * @type {function(): number}
 */
get_triangles_per_tile(TPT) :-
    data_trianglesPerTile(TPT).

/**
 * rotate_right creates a version of the input color sequence
 * that is rotated 'right' moving the 0'th element to the end (K'th) position, shifting
 * all other items down one position.
 * @type {function( number[]): number[]}
 */
rotate_right([H|T], Rotated) :-
    append(T, [H], Rotated).

/**
 * rotate_left creates a version of the input color sequence
 * that is rotated 'left' moving the last (K'th) element to the beginning (0'th) position,
 * shifting all other items up one position.
 * @type {function( number[]): number[]}
 */
rotate_left(L, [Last|Prefix]) :-
    append(Prefix, [Last], L).

/**
 * board_hash_key_coords funtion converts a pair of coordinates to a unique string used as a lookup key.
 * E.g. (2,3) becomes 'x2y3'.
 * @type {function(number, number): string}
 */
board_hash_key_coords(GridX, GridY, Key) :-
    number_codes(GridX, XCodes),
    number_codes(GridY, YCodes),
    append_lists(["x", XCodes, "y", YCodes], KeyCodes),
    atom_codes(Key, KeyCodes).

/**
 * tile_colors_match_constraint_colors function returns a true if there are no mismatches between a color constraint
 * sequence and a tile color sequence. The mismatch count of position offsets in a color constraint sequence
 * and a tile color sequence is incremented for a given position offset in the two sequences where
 * the color constraint value is >= 0 and the tile color ID is not equal to the color constraint ID.
 * In this definition any color constraint value < 0 matches any tile color ID.
 * @type {function(number[], number[]): boolean}
 */
tile_colors_match_constraint_colors(TileColors, ColorConstraints) :-
    tile_colors_mismatch_constraint_colors(TileColors, ColorConstraints, 0, 0).

tile_colors_mismatch_constraint_colors([], [], MismatchCount, MismatchCount).
tile_colors_mismatch_constraint_colors([TC|OtherTileColors], [CC|OtherColorConstraints], InputMismatchCount, MismatchCount) :-
    (CC >= 0,
     CC \= TC
      -> NextMismatchCount is InputMismatchCount + 1
    ;
     NextMismatchCount = InputMismatchCount
    ),
    tile_colors_mismatch_constraint_colors(OtherTileColors, OtherColorConstraints, NextMismatchCount, MismatchCount).

/**
 * edge_neighbor_offset function maps an 'edge' identifer (0 to 3 for squares) to a dx>dy pair to find the neighbor tile
 * position across that edge from a given tile position. From a tile at grid position (1,1), the neighbor across
 * edge 0 has (dx,dy) of (0,-1). This gives the edge 0 neighbor grid position of (1, 0).
 * @type {function(number): {dx: number, dy: number}}
 */
edge_neighbor_offset(Edge, Coordinate) :-
    data_trianglesPerTile(TPT),
    edge_neighbor_offset(TPT, Edge, Coordinate).

edge_neighbor_offset(4, Edge, Coordinate) :-
    edge_square_neighbor_offset(Edge, Coordinate).
edge_neighbor_offset(6, Edge, Coordinate) :-
    edge_hex_neighbor_offset(Edge, Coordinate).

/**
 * edge_square_neighbor_offset function maps an edge identifier 0 through 3
 * to a dX>dY displacement to shift from one tile square grid X>Y position
 * to a neighbor tile square grid X>Y position.
 * @param edge
 * @returns {{dx: number, dy: number}}
 */
edge_square_neighbor_offset(0, 0 > -1).
edge_square_neighbor_offset(1, 1 > 0).
edge_square_neighbor_offset(2, 0 > 1).
edge_square_neighbor_offset(3, -1 > 0).

/**
 * edge_hex_neighbor_offset function maps an edge identifier 0 through 5
 * to an dX>dY displacement to shift from one tile hexagon grid X>Y
 * position to a neighbor tile hexagon grid X>Y position.
 *
 * The hexagon grid is defined by two axes at a 120 degree angle.
 * The Y axis is vertical and the X axis is 120 degrees from the Y
 * axis (instead of 90 degrees).
 *
 * @param edge
 * @returns {{dx: number, dy: number}}
 */
edge_hex_neighbor_offset(0, 0 > -1).
edge_hex_neighbor_offset(1, 1 > 0).
edge_hex_neighbor_offset(2, 1 > 1).
edge_hex_neighbor_offset(3, 1 > 0).
edge_hex_neighbor_offset(4, -1 > 0).
edge_hex_neighbor_offset(5, -1 > -1).

%display_spans(_Markers, _Functor).

:- dynamic(display_spans_mode/1).

display_spans(Markers, Functor) :-
    (display_spans_mode(all);display_spans_mode(Functor))
      ->  spans(Markers, Spans),
          Structure =.. [Functor|Spans],
          writeln(Structure)
    ;
    true.

spans([H1, H2|T], Spans) :-
    spans([H2|T], H1, Spans).

spans([], _, []).
spans([H|T], R, [Span|TS]) :-
    Span is H - R,
    spans(T, H, TS).

/**
 * The 'values' function creates a displayable-on-the-console object that is the labeled values
 * returned by the parameter-defining functions of the gameModelBasics object - get_hand_color_ids,
 * get_number_of_players, and get_triangles_per_tile.
 * @type {function(): {get_hand_color_ids : number[], get_number_of_players:number, get_triangles_per_tile:number}}
 */
values([number_of_players-NOP, triangles_per_tile-TPT, hand_color_ids-HCI]) :-
    data_handColorIDSequences(HCI),
    data_numberOfPlayers(NOP),
    data_trianglesPerTile(TPT).

:- dynamic(undoable_term/2).

undoable_update(OldTerm, NewTerm) :-
    retract(OldTerm),
    asserta(NewTerm),
    (OldTerm \= NewTerm
      -> asserta(undoable_term(OldTerm, NewTerm))
    ;
    true
    ).


undo_update(Old, New) :-
    undo_update1(Old, New),
    !.

% undo_update1(OldTerm, NewTerm)
undo_update1(OldTerm, NewTerm) :-
    undoable_term(OldTerm, NewTerm)
      -> repeat,
         retract(undoable_term(OldTermX, NewTermX)),
         undo_single_update(OldTermX, NewTermX),
         OldTermX = OldTerm,
         NewTermX = NewTerm
    ;
    throw(undo_not_defined(OldTerm, NewTerm)).

% undo_single_update(OldTermX, NewTermX)
% executes a single retract/asserta.
% The cut (!) is needed to prevent retract(NewTermX)
% from endlessly succeeding on redo when OldTermX=NewTermX.

undo_single_update(OldTermX, NewTermX) :-
    retract(NewTermX),
    asserta(OldTermX),
    !.
