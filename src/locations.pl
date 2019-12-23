:- module(locations,
    [create_locations/0, save_locations/0, load_locations/0,
     save_locations_stream/1, retract_locations/0,
    create_transform_shaped_locations/3, create_mismatch_shaped_locations/1,
    clear_locations/0, clear_shaped_location_for_tile/1,
    get_shaped_positions/1,
    get_legal_positions/1, set_legal_positions/1, get_legal_positions_with_rotation/1,
    set_legal_positions_with_rotation/1, get_irreplaceables/1, set_irreplaceables/1,
    get_rebuild_positions/1, add_rebuild_position/1, find_rebuild_hole_shaped_locations/0,
    update_legal_positions/1,
    find_shaped_locations/0, incremental_find_shaped_locations/1,
    find_legal_with_rotation_locations/1, find_legal_locations/1, find_replacements/2, update_replacements/0,
    locations_values/2, %placed_position_offset/3,
    edge_to_neighbor_edge/2]).

:- use_module('../proscriptls_sdk/library/data_predicates').
:- use_module(location_model).
:- use_module(model_basics).
:- use_module(tile_model).
:- use_module(game_model_tiles).
:- use_module(library). % nth0/3

:- initialization(initdyn).

% shaped positions are places on the board where a tile could be placed without violating
% the shape constraints:
%   at least min(2,K) neighbors for board of K tiles,
%   at least one ortho neighbor,
%   placing a tile in this location would not create a 'hole' (an empty location with
%     tiles on opposite sides)
% rebuildPositions are locations that are created in the course of replacing tiles
% (moving a tile from one place in the board to another). The source position of
% the replacement becomes a 'rebuild' position.
% Once all of the replacements have been performed (usually two for transforming an edge)
% the rebuild positions that are holes (have tiles on opposite sides)
% are converted into shapedPositions. The game phase becomes rebuild_holes
% and these holes must be rebuilt from the hand first.
% Once these hole-rebuild positions have been filled, then the standard find_shaped_positions
% is run and the game phase becomes rebuild.

initdyn :-
    data_predicate_dynamics(
        [data_predicates(loc, data, [undoable],
            [locationCounter, shapedPositions, shapedPositionsComplete,
             legalPositions, legalPositionsWithRotation, irreplaceables, rebuildPositions
            ])
        ]).

dummy_reference :-
    dummy_reference,
    data_locationCounter(_).

create_locations :-
    assert_data(loc(0, [], [], [], [], [], []), 1).

save_locations_stream(Stream) :-
    save_data_stream(data, Stream).

retract_locations :-
    retract_all_data(data).

save_locations :-
    save_data(data, local_storage('mosaic')).

load_locations :-
    load_data(data, local_storage('mosaic')).

create_transform_shaped_locations(Tile, Edge, NeighborTile) :-
    clear_locations,
    get_turn(PlayerColor),
    % create constraints for replacement locations
    get_tile_colors(Tile, Colors),
    select_nth0(Edge, Colors, _, TransformColors, PlayerColor),
    edge_to_neighbor_edge(Edge, NeighborEdge),
    get_tile_colors(NeighborTile, NeighborColors),
    select_nth0(NeighborEdge, NeighborColors, _, TransformNeighborColors, PlayerColor),
    get_tile_grid_x(Tile, TX),
    get_tile_grid_y(Tile, TY),
    create_location(TileLocation, TX, TY),
    set_location_constraints(TileLocation, TransformColors),
    get_tile_grid_x(NeighborTile, NX),
    get_tile_grid_y(NeighborTile, NY),
    create_location(NeighborLocation, NX, NY),
    set_location_constraints(NeighborLocation, TransformNeighborColors),
    set_shaped_positions([TileLocation, NeighborLocation], incomplete).

create_mismatch_shaped_locations(Mismatches) :-
    clear_locations,
    % create constraints for replacement locations
    create_mismatch_locations(Mismatches, Locations),
    set_shaped_positions(Locations, incomplete).

% create_mismatch_locations(Mismatches, Locations)
create_mismatch_locations([], []).
create_mismatch_locations([H|T], [HL|TL]) :-
    create_mismatch_location(H, HL),
    create_mismatch_locations(T, TL).

create_mismatch_location(Mismatch, Location) :-
    neighbor_constraints(Mismatch, NeighborConstraints),
    get_tile_colors(Mismatch, Colors),
    adjust_mismatch_constraints(NeighborConstraints, Colors, Constraints),
    get_tile_grid_x(Mismatch, TX),
    get_tile_grid_y(Mismatch, TY),
    create_location(Location, TX, TY),
    set_location_constraints(Location, Constraints).

% adjust_mismatch_constraints(NeighborConstraints, Colors, Constraints).
adjust_mismatch_constraints([], [], []).
adjust_mismatch_constraints([H|T], [HColor|TColors], [HConstraint|TConstraints]) :-
    adjust_mismatch_constraint(H, HColor, HConstraint),
    adjust_mismatch_constraints(T, TColors, TConstraints).

adjust_mismatch_constraint(NeighborConstraint, MismatchTileColor, AdjustedConstraint) :-
    NeighborConstraint = -1
      -> MismatchTileColor = AdjustedConstraint
    ;
    NeighborConstraint = AdjustedConstraint.

neighbor_constraints(Tile, Constraints) :-
    get_triangles_per_tile(TPT),
    neighbor_constraints(TPT, Tile, Constraints).

% neighbor_constraints(NumberOfEdgesToInspect, Tile, Constraints).
% Edge is a number from 0 to (TrianglesPerTile-1)
neighbor_constraints(NumberOfEdgesToInspect, Tile, [Constraint|OtherConstraints]) :-
    Edge is NumberOfEdgesToInspect - 1,
    neighbor_constraint(Edge, Tile, Constraint),
    (Edge > 0
      -> neighbor_constraints(Edge, Tile, OtherConstraints)
    ;
    OtherConstraints = []
    ).

neighbor_constraint(Edge, Tile, Constraint) :-
    edge_neighbor_tile(Tile, Edge, NeighborID)
      -> get_tile_colors(NeighborID, NeighborColors),
         edge_to_neighbor_edge(Edge, NeighborEdge),
         nth0(NeighborEdge, NeighborColors, Constraint)
    ;
    Constraint = -1.

select_nth1(1, [Head1|Tail], Head1, [Head2|Tail], Head2) :- !.

select_nth1(N, [Head|Tail1], Item1, [Head|Tail2], Item2) :-
	nonvar(N),
	!,
	M is N-1,			% should be succ(M, N)
	select_nth1(M, Tail1, Item1, Tail2, Item2).

select_nth1(N,[Head|Tail1], Item1, [Head|Tail2], Item2) :-
	var(N),			% select_nth1(-,+,+,?,?) or select_nth1(-,?,?,+,+)
	select_nth1(M, Tail1, Item1, Tail2, Item2),
	N is M + 1.

select_nth0(0, [Head1|Tail], Head1, [Head2|Tail], Head2) :- !.

select_nth0(N, [Head|Tail1], Item1, [Head|Tail2], Item2) :-
	nonvar(N),
	!,
	M is N-1,			% should be succ(M, N)
	select_nth0(M, Tail1, Item1, Tail2, Item2).

select_nth0(N,[Head|Tail1], Item1, [Head|Tail2], Item2) :-
	var(N),			% select_nth1(-,+,+,?,?) or select_nth1(-,?,?,+,+)
	select_nth0(M, Tail1, Item1, Tail2, Item2),
	N is M + 1.

clear_locations :-
    setof(Location, X^get_location_grid_x(Location, X), Locations)
      -> clear_locations(Locations),
         set_shaped_positions([], incomplete),
         set_legal_positions_with_rotation([]),
         set_legal_positions([])
    ;
    true.

clear_unused_locations :-
    setof(Location, X^get_location_grid_x(Location, X), Locations),
    get_shaped_positions(UsedLocations),
    subtract(Locations, UsedLocations, UnusedLocations),
    clear_locations(UnusedLocations).

clear_shaped_location_for_tile(Tile) :-
    get_tile_grid_x(Tile, X),
    get_tile_grid_y(Tile, Y),
    get_location_grid_x(Location, X),
    get_location_grid_y(Location, Y),
    clear_replacements_from_location(Location),
    clear_location(Location),
    clear_location_from_shaped(Location).

% clear_replacements_from_location(Location)
% clears the tiles listed in replacements that
% are no longer needed for any shaped location
% other than Location.
clear_replacements_from_location(Location) :-
    get_location_replacements(Location, LocalReplacements),
    get_shaped_positions(Shaped),
    delete(Shaped, Location, OtherShaped),
    tiles_from_location_replacements(LocalReplacements, LocalTiles),
    tiles_not_replacing_locations(LocalTiles, OtherShaped, NotReplacing),
    get_replacements(OldReplacements),
    subtract(OldReplacements, NotReplacing, NewReplacements),
    set_replacements(NewReplacements).

% replacements_independent_of_location(Replacements, Shaped, Location, Independent)
tiles_not_replacing_locations(Tiles, [], Tiles) :-
    !.
tiles_not_replacing_locations([], _Shaped, []).
tiles_not_replacing_locations([H|T], Shaped, Independent) :-
    tile_not_replacing_locations(Shaped, H, Independent, Tail),
    tiles_not_replacing_locations(T, Shaped, Tail).

tile_not_replacing_locations([], Tile, [Tile|Tail], Tail).
tile_not_replacing_locations([H|T], Tile, NotReplacing, Tail) :-
    tile_can_replace_location(H, Tile)
      -> NotReplacing = Tail
    ;
    tile_not_replacing_locations(T, Tile, NotReplacing, Tail).

tile_can_replace_location(ShapedLocation, Tile) :-
    get_location_replacements(ShapedLocation, LocalReplacements),
    tiles_from_location_replacements(LocalReplacements, Tiles),
    member(Tile, Tiles).

clear_hole_inducing_shaped_locations(Tile) :-
    get_tile_grid_x(Tile, TX),
    get_tile_grid_y(Tile, TY),
    check_tile_ortho_neighbors(TX > TY).

check_tile_ortho_neighbors(TilePosition) :-
    check_tile_ortho_neighbors([-1 > 0, 1 > 0, 0 > -1, 0 > 1], TilePosition).

check_tile_ortho_neighbors([], _TilePosition).
check_tile_ortho_neighbors([H|T], TilePosition) :-
    check_tile_ortho_neighbor(H, TilePosition),
    check_tile_ortho_neighbors(T, TilePosition).

check_tile_ortho_neighbor(DX > DY, TX > TY) :-
    LX is DX + TX,
    LY is DY + TY,
    get_location_grid_x(Location, LX),
    get_location_grid_y(Location, LY)
      -> (L2X is LX + DX,
          L2Y is LY + DY,
          get_location_grid_x(Location2, L2X),
          get_location_grid_y(Location2, L2Y)
           -> clear_location(Location2),
              clear_location_references(Location2)
              %writeln(clear_hole_inducing_location(Location2))
         ;
           true
         )
    ;
    true.

clear_location_references(Location) :-
    clear_location_from_shaped(Location),
    clear_location_from_legal_positions_with_rotation(Location),
    clear_location_from_legal_positions(Location).

clear_location_from_shaped(Location) :-
    get_shaped_positions(Shaped, Complete),
    delete(Shaped, Location, OtherShaped)
      -> set_shaped_positions(OtherShaped, Complete)
    ;
     true.

clear_location_from_legal_positions_with_rotation(Location) :-
    get_legal_positions_with_rotation(LPWR),
    delete(LPWR, Location, OtherLPWR)
      -> set_legal_positions_with_rotation(OtherLPWR)
    ;
    true.

clear_location_from_legal_positions(Location) :-
    get_legal_positions(LP),
    delete(LP, Location, OtherLP)
      -> set_legal_positions(OtherLP)
    ;
    true.

clear_locations([]).
clear_locations([H|T]) :-
    clear_location(H),
    clear_locations(T).

reset_by_last_tile_placed([]).
reset_by_last_tile_placed([H|T]) :-
    set_location_by_last_tile_placed(H, false),
    reset_by_last_tile_placed(T).

increment_location_counter(NewCounter) :-
    data_default_id(DataID),
    data_locationCounter(DataID, Counter),
    NewCounter is Counter + 1,
    update_data_locationCounter(DataID, Counter, NewCounter).

get_shaped_positions(Value) :-
    data_shapedPositions(Value).

get_shaped_positions(Value, Complete) :-
    data_shapedPositions(Value),
    data_shapedPositionsComplete(Complete).

set_shaped_positions(Value, Complete) :-
    data_default_id(ID),
    set_data_shapedPositions(ID, Value),
    set_data_shapedPositionsComplete(ID, Complete).

get_legal_positions(Value) :-
    data_legalPositions(Value).

set_legal_positions(Value) :-
    data_default_id(ID),
    set_data_legalPositions(ID, Value).

get_legal_positions_with_rotation(Value) :-
    data_legalPositionsWithRotation(Value).

set_legal_positions_with_rotation(Value) :-
    data_default_id(ID),
    set_data_legalPositionsWithRotation(ID, Value).

get_irreplaceables(Value) :-
    data_irreplaceables(Value).

set_irreplaceables(Value) :-
    data_default_id(ID),
    set_data_irreplaceables(ID, Value).

get_rebuild_positions(RebuildPositions) :-
    data_rebuildPositions(RebuildPositions).

add_rebuild_position(X > Y) :-
    create_location(LocationID, X, Y),
    data_default_id(ID),
    update_data_rebuildPositions(ID, Rebuilds, [LocationID|Rebuilds]).

set_rebuild_positions(Value) :-
    data_default_id(ID),
    set_data_rebuildPositions(ID, Value).

find_rebuild_hole_shaped_locations :-
    data_rebuildPositions(RebuildPositions),
    set_rebuild_positions([]),
    keep_hole_locations(RebuildPositions, HoleLocations),
    !,
    HoleLocations \= [],
    set_shaped_positions(HoleLocations, incomplete).

keep_hole_locations([], []).
keep_hole_locations([H|T], HoleLocations) :-
    keep_hole_location(H, HoleLocations, HoleLocationsTail),
    keep_hole_locations(T, HoleLocationsTail).

keep_hole_location(Location, HoleLocations, HoleLocationsTail) :-
    get_location_grid_x(Location, X),
    get_location_grid_y(Location, Y),
    position_is_hole(X>Y)
      -> determine_color_constraints_for_location(Location, Constraints),
         set_location_constraints(Location, Constraints),
         HoleLocations = [Location|HoleLocationsTail]
    ;
    clear_location(Location),
    HoleLocations = HoleLocationsTail.

update_legal_positions(Colors) :-
    data_legalPositionsWithRotation(PositionsWithRotations),
    filter_legal_positions_with_rotation_by_tile_colors(PositionsWithRotations, Colors, ConstrainedPositions),
    set_legal_positions(ConstrainedPositions).

% Shaped locations are a function purely of the
% current board tiles, independent of a particular tile being placed.
% find_shaped_locations/0 should be invoked every time the board is changed.

find_shaped_locations :-
    get_board(Board),
    (Board = []
      -> create_location(ID, 0, 0),
         set_shaped_positions([ID], complete)
    ;
     wam_duration(Start),
     candidate_spaces(Board, Candidates),
     wam_duration(Mark1),
     shaped_candidates(Candidates, ShapedLocations),
     wam_duration(Mark2),
     set_shaped_positions(ShapedLocations, complete),
     wam_duration(End),
     !,
     display_spans([Start, Mark1, Mark2, End], find_shaped_locations)
    ).

% if the board grew to 2 tiles with the addition of ID
% then recalculate all shape locations - the old shape
% locations were calculated with a neighbor count requirement
% of at least 1 and the current shape locations must have a
% neighbor count of at least 2.

incremental_find_shaped_locations(ID) :-
     get_board(B),
     length(B, L),
     ((L = 2
      ;
       L > 2,
       get_shaped_positions([])
      )
       -> clear_locations,
          find_shaped_locations
     ;
     get_shaped_positions([_|_], incomplete)
       -> true
     ;
     wam_duration(Start),
     clear_hole_inducing_shaped_locations(ID), % some old shaped locations may have become 'hole inducing' due to new Tile on board.
     incremental_candidate_spaces(ID, Candidates),
     wam_duration(Mark1),
     shaped_candidates(Candidates, NewShapedLocations),
     wam_duration(Mark2),
     get_shaped_positions(OldShapedLocations, Complete),
     append(NewShapedLocations, OldShapedLocations, ShapedLocations),
     set_shaped_positions(ShapedLocations, Complete),
     clear_unused_locations,
     wam_duration(End),
     !,
     display_spans([Start, Mark1, Mark2, End], find_shaped_locations)
     ).

% Legal-with-rotation locations are shaped locations that
% a particular tile can occupy in some rotation (not necessarily
% the rotation that tile currently has).
% If there are *any* legal-with-rotation locations that are
% orthogonally adjacent to the last-placed tile then
% *only* such orthogonally adjacent positions are legal.
% find_legal_with_rotation_locations/1 should be invoked whenever the
% tile selection is changed.

find_legal_with_rotation_locations(Tile) :-
    get_game_phase(rebuild),
    get_shaped_positions(ShapedLocations),
    findall(ShapedLocation,
        (member(ShapedLocation, ShapedLocations),
         get_location_replacements(ShapedLocation, ShapedReplacements),
         (member(Tile-_ColorsList, ShapedReplacements) % created for mismatch replacements
         ;
          member(Tile, ShapedReplacements) % created for exact replacements
         )
        ),
        LegalWithRotation
       ),
    LegalWithRotation \= [],
    !,
    set_legal_positions_with_rotation(LegalWithRotation).
find_legal_with_rotation_locations(Tile) :-
    get_shaped_positions(ShapedLocations),
    get_tile_colors(Tile, TileColors),
    filter_shaped_for_legal_locations(ShapedLocations, TileColors, LegalWithRotation),
    (get_game_phase(build)
      -> filter_by_last_tile_placed(LegalWithRotation, LegalByLastTilePlaced),
         length(LegalByLastTilePlaced, LegalByLastLength)
     ;
     LegalByLastLength = 0
    ),
    (LegalByLastLength > 0
        -> FinalLegalWithRotation = LegalByLastTilePlaced
    ;
     FinalLegalWithRotation = LegalWithRotation
    ),
    set_legal_positions_with_rotation(FinalLegalWithRotation).

% Legal locations for selected Tile are those locations where Tile
% as currently oriented may be placed on the board - it will
% match colors, satisfy the board shape constraints, and
% be next to the last placed tile if possible for the selected Tile.
% find_legal_locations/1 should be invoked whenever a tile is selected
% or rotated.

find_legal_locations(Tile) :-
    get_game_phase(rebuild),
    get_shaped_positions(ShapedLocations),
    get_tile_colors(Tile, TileColors),
    findall(ShapedLocation,
        (member(ShapedLocation, ShapedLocations),
         get_location_replacements(ShapedLocation, ShapedReplacements),
         member(Tile-ColorsList, ShapedReplacements),
         member(TileColors, ColorsList)
        ),
        Legal
       ),
    Legal \= [],
    !,
    set_legal_positions(Legal).
find_legal_locations(Tile) :-
    get_legal_positions_with_rotation(LegalWithRotation),
    get_tile_colors(Tile, TileColors),
    filter_legal_positions_with_rotation_by_tile_colors(LegalWithRotation, TileColors, Legal),
    set_legal_positions(Legal).

candidate_spaces(Board, Candidates) :-
    wam_duration(Start),
    candidate_spaces(Board, [], KeyedCandidates),
    wam_duration(Mark1),
    dekey_list(KeyedCandidates, PossibleCandidates),
    wam_duration(Mark2),
    remove_nonorthogonal(PossibleCandidates, Candidates),
    wam_duration(End),
    !,
    display_spans([Start,Mark1,Mark2,End], candidate_spaces).

incremental_candidate_spaces(NewBoardTile, NewCandidates) :-
    wam_duration(Start),
    get_shaped_positions(OldShapedPositions),
    reset_by_last_tile_placed(OldShapedPositions),
    key_list(OldShapedPositions, OldKeyedCandidates),
    candidate_spaces([NewBoardTile], OldKeyedCandidates, KeyedCandidates),
    wam_duration(Mark1),
    dekey_list(KeyedCandidates, PossibleCandidates),
    subtract(PossibleCandidates, OldShapedPositions, NewPossibleCandidates),
    extend_neighbors(NewPossibleCandidates, NewBoardTile),
    wam_duration(Mark2),
    remove_nonorthogonal(NewPossibleCandidates, NewCandidates),
    wam_duration(End),
    !,
    display_spans([Start,Mark1,Mark2,End], incremental_candidate_spaces).

remove_nonorthogonal([], []).
remove_nonorthogonal([H|T], Candidates) :-
    remove_nonorthogonal1(H, Candidates, Next),
    remove_nonorthogonal(T, Next).

remove_nonorthogonal1(PossibleCandidate, Candidates, Next) :-
    get_location_orthogonal_neighbors(PossibleCandidate, OrthogonalNeighborsCount),
    (OrthogonalNeighborsCount > 0
      -> Candidates = [PossibleCandidate|Next]
    ;
     Candidates = Next
    ).

key_list([], []).
key_list([H|T], [K-H|TK]) :-
    get_location_grid_x(H, X),
    get_location_grid_y(H, Y),
    board_hash_key_coords(X, Y, K),
    key_list(T, TK).

dekey_list([], []).
dekey_list([_-H|T], [H|TT]) :-
    dekey_list(T, TT).

tiles_from_location_replacements([], []).
tiles_from_location_replacements([H|T], [Tile|TK]) :-
    (H = Tile-_
      -> true
    ;
    H = Tile
    ),
    tiles_from_location_replacements(T, TK).

candidate_spaces([], Candidates, Candidates).
candidate_spaces([H|T], CandidatesSoFar, CandidatesTotal) :-
    candidate_space(H, CandidatesSoFar, CandidatesNext),
    candidate_spaces(T, CandidatesNext, CandidatesTotal).

candidate_space(PlacedTile, CandidatesIn, CandidatesOut) :-
    wam_duration(Start),
    check_neighbors1([-1,0,1], PlacedTile, CandidatesIn, CandidatesOut),
    wam_duration(End),
    !,
    display_spans([Start, End], candidate_space).

check_neighbors1([], _PlacedTile, Candidates, Candidates).
check_neighbors1([H|T], PlacedTile, CandidatesIn, CandidatesOut) :-
    check_neighbors2([-1,0,1], H, PlacedTile, CandidatesIn, CandidatesNext),
    check_neighbors1(T, PlacedTile, CandidatesNext, CandidatesOut).

check_neighbors2([], _, _, Candidates, Candidates).
check_neighbors2([H|T], DX, PlacedTile, CandidatesIn, CandidatesOut) :-
    check_neighbors3(H, DX, PlacedTile, CandidatesIn, CandidatesNext),
    check_neighbors2(T, DX, PlacedTile, CandidatesNext, CandidatesOut).

check_neighbors3(DY, DX, PlacedTile, CandidatesIn, CandidatesOut) :-
    get_tile_grid_x(PlacedTile, PlacedX),
    get_tile_grid_y(PlacedTile, PlacedY),
    TX is PlacedX + DX,
    TY is PlacedY + DY,
    (\+ get_board_tile_by_grid(TX > TY, _NeighborTile)
      -> check_neighbor(PlacedTile, TX, TY, DX, DY, CandidatesIn, CandidatesOut)
    ;
    CandidatesIn = CandidatesOut
    ).

check_neighbor(PlacedTile, TX, TY, DX, DY, CandidatesIn, CandidatesOut) :-
    wam_duration(Start),
    board_hash_key_coords(TX, TY, Key),
    (member(Key-ID, CandidatesIn)
      -> CandidatesIn = CandidatesOut
    ;
     create_location(ID, TX, TY),
     [Key-ID|CandidatesIn] = CandidatesOut
    ),
    increment_location_neighbors(ID, _NewCount),
    ((DX = 0; DY = 0)
      -> check_orthogonal_neighbor(PlacedTile, DX, DY, ID)
    ;
     true
    ),
    wam_duration(End),
    !,
    display_spans([Start, End], check_neighbor).

check_orthogonal_neighbor(PlacedTile, DX, DY, ID) :-
    wam_duration(Start),
    increment_location_orthogonal_neighbors(ID, _NewCount),
    %placed_position_offset(DX, DY, PlacedPositionOffset),
    edge_neighbor_offset(PlacedPositionOffset, DX>DY),
    edge_to_neighbor_edge(PlacedPositionOffset, ConstrainedOffset),
    ConstrainedPosition is ConstrainedOffset + 1,
    get_tile_colors(PlacedTile, PlacedColors),
    wam_duration(Mark),
    nth0(PlacedPositionOffset, PlacedColors, ColorConstraint),
    set_location_constraint(ID, ConstrainedPosition, ColorConstraint),
    (get_game_phase(build),
     get_last_build_phase_tile_placed(PlacedTile)
      -> set_location_by_last_tile_placed(ID, true)
    ;
    true
    ),
    wam_duration(End),
    !,
    display_spans([Start, Mark, End], check_orthogonal_neighbor).

create_location(ID, GridX, GridY) :-
    increment_location_counter(ID),
    create_location_model(ID, GridX, GridY).


extend_neighbors([], _NewTile).
extend_neighbors([Candidate|OtherCandidates], NewTile) :-
    extendDX([-1,0,1], Candidate, NewTile),
    extend_neighbors(OtherCandidates, NewTile).

extendDX([], _C, _NewTile).
extendDX([H|T], C, NewTile) :-
    extendDY([-1,0,1], H, C, NewTile),
    extendDX(T, C, NewTile).

extendDY([], _DX, _C, _NewTile).
extendDY([H|T], DX, C, NewTile) :-
    extendDXDY(DX, H, C, NewTile),
    extendDY(T, DX, C, NewTile).

% check_orthogonal_neighbor(Tile, TDX, TDY, C)
% expects that TDX and TDY are the delta X and Y
% to transform the Candidate (C) position to the
% Tile position.
% The DX/DY input to extendDXDY are the delta X and Y
% to transform C position to Tile position.
% DX/DY is negated to create TDX/TDY for the
% input to check_orthogonal_neighbor/4.

extendDXDY(DX, DY, C, NewTile) :-
    get_location_grid_x(C, X),
    get_location_grid_y(C, Y),
    TX is X + DX,
    TY is Y + DY,
    (get_tile_grid_x(Tile, TX),
     get_tile_grid_y(Tile, TY),
     (Tile \= NewTile
      -> increment_location_neighbors(C, _NewCount),
         ((DX = 0; DY = 0)
           -> TDX is -1 * DX,
              TDY is -1 * DY,
              check_orthogonal_neighbor(Tile, TDX, TDY, C)
         ;
          true
         )
     ;
      true
     )
    ;
    true
    ).

shaped_candidates([], []).
shaped_candidates([H|T], ShapedLocations) :-
    shaped_candidate(H, ShapedLocations, ShapedLocationsTail),
    shaped_candidates(T, ShapedLocationsTail).

shaped_candidate(Candidate, ShapedLocations, ShapedLocationsTail) :-
    get_location_constraints(Candidate, Constraints),
    ((Constraints = []
     ;
      get_location_neighbors(Candidate, Neighbors),
      get_board(Board),
      length(Board, Length),
      Neighbors < min(2, Length)
     ;
      candidate_would_create_hole(Constraints, Candidate)
     )
      -> ShapedLocations = ShapedLocationsTail
    ;
    true
      -> ShapedLocations = [Candidate|ShapedLocationsTail]
    ).

candidate_would_create_hole(Constraints, Candidate) :-
    candidate_would_create_hole(Constraints, 0, Candidate).

candidate_would_create_hole([H|T], ConstraintOffset, Candidate) :-
    candidate_would_create_hole1(H, ConstraintOffset, Candidate)
      -> true
    ;
    NextConstraintOffset is ConstraintOffset + 1,
    candidate_would_create_hole(T, NextConstraintOffset, Candidate).

% -1 implies there is no tile on edge/constraint at constraintOfst.
% Check if there is a tile the next space beyond.
% If there is, then placing in this location would create
% a shape violation 'hole' in the board.

candidate_would_create_hole1(-1, ConstraintOffset, Candidate) :-
    location_edge_second_neighbor_tile(Candidate, ConstraintOffset, _).

% position_is_hole(X > Y) holds when there are tiles on opposite sides
% of the position X > Y.
position_is_hole(X > Y) :-
    (Yp1 is Y+1, Ym1 is Y-1,
     tiles_exist_at_positions([X > Yp1, X > Ym1])
    ;
     Xp1 is X+1, Xm1 is X-1,
     tiles_exist_at_positions([Xp1 > Y, Xm1 > Y])
    ),
    !.

tiles_exist_at_positions([]).
tiles_exist_at_positions([H|T]) :-
    tile_exists_at_position(H),
    tiles_exist_at_positions(T).

tile_exists_at_position(X>Y) :-
    get_tile_grid_x(Tile, X),
    get_tile_grid_y(Tile, Y).

filter_shaped_for_legal_locations([], _, []).
filter_shaped_for_legal_locations([H|T], TileColors, LegalWithRotation) :-
    filter_shaped_for_legal_location(H, TileColors, LegalWithRotation, LegalWithRotationTail),
    filter_shaped_for_legal_locations(T, TileColors, LegalWithRotationTail).

filter_shaped_for_legal_location(H, TileColors, LegalWithRotation, LegalWithRotationTail) :-
    get_location_constraints(H, ColorConstraint),
    (matching_rotations(TileColors, ColorConstraint, _)
      -> LegalWithRotation = [H|LegalWithRotationTail]
    ;
     LegalWithRotation = LegalWithRotationTail
    ).

% filter_by_last_tile_placed(LegalWithRotation, LegalByLastTilePlaced).
filter_by_last_tile_placed([], []).
filter_by_last_tile_placed([H|T], LegalByLastTilePlaced) :-
    filter_by_last_tile_placed1(H, LegalByLastTilePlaced, LegalByLastTilePlacedTail),
    filter_by_last_tile_placed(T, LegalByLastTilePlacedTail).

filter_by_last_tile_placed1(Location, LegalByLastTilePlaced, LegalByLastTilePlacedTail) :-
    get_location_by_last_tile_placed(Location, true)
      -> LegalByLastTilePlaced = [Location|LegalByLastTilePlacedTail]
    ;
    LegalByLastTilePlaced = LegalByLastTilePlacedTail.

% filter_legal_positions_with_rotation_by_tile(LegalWithRotation, Tile, Legal)
filter_legal_positions_with_rotation_by_tile_colors([], _TileColors, []).
filter_legal_positions_with_rotation_by_tile_colors([H|T], TileColors, Legal) :-
    filter_legal_position_with_rotation_by_tile_colors(H, TileColors, Legal, LegalTail),
    filter_legal_positions_with_rotation_by_tile_colors(T, TileColors, LegalTail).

filter_legal_position_with_rotation_by_tile_colors(LegalWithRotationLocation, TileColors, Legal, LegalTail) :-
    get_location_constraints(LegalWithRotationLocation, Constraints),
    (tile_colors_match_constraint_colors(TileColors, Constraints)
      -> Legal = [LegalWithRotationLocation|LegalTail]
    ;
     Legal = LegalTail
    ).

location_edge_second_neighbor_tile(ID, Edge, NeighborTile) :-
    location_edge_second_neighbor_position(ID, Edge, Position),
    get_board_tile_by_grid(Position, NeighborTile).

matching_rotations(Colors, Constraint, Rotation) :-
    rotate_left(Colors, FinalColors),
    matching_rotations(Colors, FinalColors, Constraint, Rotation).

matching_rotations(Colors, FinalColors, Constraint, Rotation) :-
    tile_colors_match_constraint_colors(Colors, Constraint)
      -> Colors = Rotation
    ;
    Colors \= FinalColors,
    rotate_right(Colors, NextColors),
    matching_rotations(NextColors, FinalColors, Constraint, Rotation).

locations_values(ID, Values) :-
    labelled_values(data, ID, Values).

% Find replacement tiles for the given locations.
% A replacement tile for a location has colors that can be rotated to match
% the constraints of that location.
% The replacement tiles are recorded using set_replacements/1.

find_replacements(Tiles, Locations) :-
    find_exact_replacements(Tiles, Locations, TileLocationReplacementMap), % TileLocationReplacementMap = [Tile-Locations, ...]
    invert_map(TileLocationReplacementMap, ReplacementMap),
    irreplaceable_locations(Locations, ReplacementMap, IrreplaceableLocations), % ReplacementMap = [Location-Tiles, ...]
    (IrreplaceableLocations = []
      -> replacement_map_tiles(ReplacementMap, ReplacementTiles)
    ;
     find_minimal_mismatch_replacements(IrreplaceableLocations, Tiles, ReplacementTiles)
    ),
    set_replacements(ReplacementTiles).

update_replacements :-
    get_replacements(OldReplacements),
    OldReplacements = []
      -> true
    ;
    get_shaped_positions(TransformShapedLocations),
    (TransformShapedLocations = []
      -> set_replacements([])
    ;
    get_board(BoardTiles),
    find_replacements(BoardTiles, TransformShapedLocations)
    ).

% replacement_map_tiles(ReplacementMap, Replacements) processes ReplacementMap
% to register replacement tiles for each location and
% to determine the list of (distinct) tiles for Replacements.
% The ReplacementMap is a list of Location-Tiles pairs where the Location may be
% replaced without any color mismatch by some rotation of each tile in Tiles.

replacement_map_tiles(ReplacementMap, Replacements) :-
    replacement_map_tiles1(ReplacementMap, RawReplacements),
    sort(RawReplacements, Replacements). % eliminate duplicates.

replacement_map_tiles1([], []).
replacement_map_tiles1([Location-Tiles|MapTail], Replacements) :-
    set_location_replacements(Location, Tiles),
    append(Tiles, ReplacementsTail, Replacements),
    replacement_map_tiles1(MapTail, ReplacementsTail).

invert_map(Map, InvertMap) :-
    invert_map(Map, [], InvertMap).

% invert_map(Map, InvertSoFar, InvertMap) inverts the list Map where the
% elements are of the form A-BList and InvertSoFar and InvertMap have
% inverted elements of the form B-AList. The elements of Map are used
% to extend InvertSoFar to create InvertMap.

invert_map([], Invert, Invert).
invert_map([K-V|T], InvertIn, InvertOut) :-
    invert_map1(V, K, InvertIn, InvertNext),
    invert_map(T, InvertNext, InvertOut).

% invert_map1(BList, A, InvertSoFar, Invert) extends InvertSoFar to create
% Invert by adding A to the AList of each B-AList element of InvertSoFar and
% creating a new element B-[A] where there is no element for B in InvertSoFar.
invert_map1([], _, Invert, Invert).
invert_map1([H|T], K, InvertIn, InvertOut) :-
    (select(H-KVIn, InvertIn, H-KVNext, InvertNext)
      -> KVNext = [K|KVIn]
    ;
     InvertNext = [H-[K]|InvertIn]
    ),
    invert_map1(T, K, InvertNext, InvertOut).

% find_exact_replacements(Tiles, Locations, TileLocationReplacementMap) records 
% for each Tile in Tiles the MatchLocations in Locations for which some 
% rotation of Tile may replace a MatchLocation without any color mismatch.
% If a Tile in Tiles has no MatchLocations then it is not recorded in TileLocationReplacementMap.
find_exact_replacements([], _Locations, []).
find_exact_replacements([H|T], Locations, ReplacementMap) :-
    find_exact_replacement(H, Locations, ReplacementMap, MapTail),
    find_exact_replacements(T, Locations, MapTail).

% find_exact_replacement(Tile, Locations, ReplacementMap, ReplacementMapTail)
% records the MatchLocations in Locations for which some
% rotation of Tile may replace a MatchLocation without any color mismatch.
% If a Tile in Tiles has no MatchLocations then it is not recorded in TileLocationReplacementMap.
find_exact_replacement(Tile, Locations, ReplacementMap, ReplacementMapTail) :-
    get_tile_colors(Tile, Colors),
    rotate_left(Colors, LastRotation),
    find_exact_replacement1(Colors, LastRotation, Locations, Matches),
    (Matches = []
      -> ReplacementMap = ReplacementMapTail
    ;
    sort(Matches, SortedMatches), % remove duplicates
    ReplacementMap = [Tile-SortedMatches|ReplacementMapTail]
    ).

% find_exact_replacement1(Colors, LastRotation, Locations, Matches)
% records the MatchLocations in Locations for which some right
% rotation of Colors (up to and including LastRotation) matches the MatchLocation's color constraint.
% The same Location may be matched multiple times (i.e. by multiple rotations of Colors)
% if that location's constraint includes 'match-any' values of -1.
find_exact_replacement1(Colors, LastRotation, Locations, Matches) :-
    find_exact_replacement2(Locations, Colors, Matches, MatchesTail),
    (Colors \= LastRotation
      ->  rotate_right(Colors, NextColors),
          find_exact_replacement1(NextColors, LastRotation, Locations, MatchesTail)
    ;
     MatchesTail = []
    ).

% find_exact_replacement2(Locations, Colors, Matches, MatchesTail)
% records the MatchLocations in Locations for which
% Colors matches the MatchLocation's color constraint.
find_exact_replacement2([], _, Matches, Matches).
find_exact_replacement2([H|T], Colors, Matches, MatchesTail) :-
    find_exact_replacement3(H, Colors, Matches, MatchesNext),
    find_exact_replacement2(T, Colors, MatchesNext, MatchesTail).

% find_exact_replacement3(Location, Colors, Matches, MatchesTail)
% determines if Location's constraints match Colors and if so then
% Matches = [Location|MatchesTail]. Otherwise, Matches = MatchesTail.
find_exact_replacement3(Location, Colors, Matches, MatchesTail) :-
    get_location_constraints(Location, Constraints),
    matching_rotations(Colors, Constraints, _)
      -> Matches = [Location|MatchesTail]
    ;
    Matches = MatchesTail.

irreplaceable_locations([], _, []).
irreplaceable_locations([H|T], ReplacementMap, IrreplaceableLocations) :-
    (member(H-_, ReplacementMap)
      -> IrreplaceableLocations = Next
    ;
    IrreplaceableLocations = [H|Next],
    set_location_minimum_mismatch(H, 1000) % arbitrary large value - all possible mismatch counts are less than this.
    ),
    irreplaceable_locations(T, ReplacementMap, Next).

% find_minimal_mismatch_replacements(IrreplaceableLocations, ReplacementMap, Tiles, ReplacementTiles).
find_minimal_mismatch_replacements(IrreplaceableLocations, Tiles, SortedReplacementTiles) :-
    find_minimal_mismatch_replacements_for_locations(IrreplaceableLocations, Tiles, ReplacementTiles),
    sort(ReplacementTiles, SortedReplacementTiles). % remove duplicates - a tile may be a replacement for more than one location.

find_minimal_mismatch_replacements_for_locations([], _Tiles, []).
find_minimal_mismatch_replacements_for_locations([H|T], Tiles, ReplacementTiles) :-
    find_minimal_mismatch_replacements_for_tiles_at_location(Tiles, H, ReplacementTiles, NextReplacementTiles),
    find_minimal_mismatch_replacements_for_locations(T, Tiles, NextReplacementTiles).

find_minimal_mismatch_replacements_for_tiles_at_location([], _IrreplaceableLocation, Replacements, Replacements).
find_minimal_mismatch_replacements_for_tiles_at_location([H|T], IrreplaceableLocation, Replacements, ReplacementsTail) :-
    find_minimal_mismatch_replacements_for_tile_at_location(H, IrreplaceableLocation, Replacements, Next),
    find_minimal_mismatch_replacements_for_tiles_at_location(T, IrreplaceableLocation, Next, ReplacementsTail).

find_minimal_mismatch_replacements_for_tile_at_location(Tile, IrreplaceableLocation, Replacements, ReplacementsTail) :-
    get_tile_colors(Tile, Colors),
    rotate_left(Colors, FinalColors),
    find_minimal_mismatch_replacements_for_color_rotations_of_tile(Colors, FinalColors, Tile, IrreplaceableLocation),
    get_location_replacements(IrreplaceableLocation, LocationReplacements),
    tiles_from_location_replacements(LocationReplacements, LocationReplacementTiles),
    append(LocationReplacementTiles, ReplacementsTail, Replacements).

find_minimal_mismatch_replacements_for_color_rotations_of_tile(FinalColors, FinalColors, Tile, IrreplaceableLocation) :-
    !,
    find_minimal_mismatch_replacements_for_color_rotation_of_tile(FinalColors, Tile, IrreplaceableLocation).
find_minimal_mismatch_replacements_for_color_rotations_of_tile(Colors, FinalColors, Tile, IrreplaceableLocation) :-
    Colors \= FinalColors,
    find_minimal_mismatch_replacements_for_color_rotation_of_tile(Colors, Tile, IrreplaceableLocation),
    rotate_right(Colors, NextColors),
    find_minimal_mismatch_replacements_for_color_rotations_of_tile(NextColors, FinalColors, Tile, IrreplaceableLocation).

find_minimal_mismatch_replacements_for_color_rotation_of_tile(Colors, Tile, IrreplaceableLocation) :-
    get_location_constraints(IrreplaceableLocation, Constraints),
    tile_colors_mismatch_constraint_colors(Colors, Constraints, 0, Mismatch),
    get_location_minimum_mismatch(IrreplaceableLocation, MinimumMismatch),
    (Mismatch < MinimumMismatch
      -> set_location_replacements(IrreplaceableLocation, [Tile-[Colors]]),
         set_location_minimum_mismatch(IrreplaceableLocation, Mismatch)
    ;
     Mismatch = MinimumMismatch,
     get_location_replacements(IrreplaceableLocation, LocationReplacements)
      -> (select(Tile-ColorsList, LocationReplacements, Tile-NewColorsList, NewLocationReplacements)
           -> (member(Colors, ColorsList)
               -> ColorsList = NewColorsList
              ;
              [Colors|ColorsList] = NewColorsList
              )
         ;
          NewLocationReplacements = [Tile-[Colors]|LocationReplacements]
         ),
         set_location_replacements(IrreplaceableLocation, NewLocationReplacements)
    ;
    true % skip Tile: Mismatch > MinimumMismatch.
    ).

% get color constraints for facing edge of tiles at -1,0 1,0 0,-1 0,1  relative to Location.
determine_color_constraints_for_location(Location, Constraints) :-
    determine_constraints([-1>0, 1>0, 0> -1, 0>1], Location, KeyedConstraints),
    sort(KeyedConstraints, SortedKeyedConstraints),
    dekey_list(SortedKeyedConstraints, Constraints).

determine_constraints([], _Location, []).
determine_constraints([H|T], Location, [HC|TC]) :-
    determine_constraints1(H, Location, HC),
    determine_constraints(T, Location, TC).

determine_constraints1(CoordinateDelta, Location, LocationEdge-Constraint) :-
    edge_neighbor_offset(LocationEdge, CoordinateDelta),
    edge_to_neighbor_edge(LocationEdge, NeighborEdge),
    (location_neighbor_tile(Location, CoordinateDelta, NeighborTile)
       -> get_tile_colors(NeighborTile, NeighborColors),
          nth0(NeighborEdge, NeighborColors, Constraint)
     ;
     Constraint = -1
    ).

location_neighbor_tile(Location, DX>DY, NeighborTile) :-
    get_location_grid_x(Location, LX),
    get_location_grid_y(Location, LY),
    NX is LX + DX,
    NY is LY + DY,
    get_tile_grid_x(NeighborTile, NX),
    get_tile_grid_y(NeighborTile, NY).
