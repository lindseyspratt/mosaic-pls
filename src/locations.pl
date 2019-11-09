:- module(locations, [create_locations/0,
    clear_locations/0, clear_shaped_location_for_tile/1,
    get_shaped_positions/1,
    get_legal_positions/1, set_legal_positions/1, get_legal_positions_with_rotation/1,
    set_legal_positions_with_rotation/1, get_irreplaceables/1, set_irreplaceables/1, update_legal_positions/1,
    find_shaped_locations/0, incremental_find_shaped_locations/1,
    find_legal_with_rotation_locations/1, find_legal_locations/1, locations_values/2 ]).

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

initdyn :-
    data_predicate_dynamics([data_predicates(loc, data,
        [locationCounter, shapedPositions, legalPositions,legalPositionsWithRotation,irreplaceables])]).

dummy_reference :-
    dummy_reference,
    data_locationCounter(_).

create_locations :-
    assert_data(loc(0, [], [], [], []), 1).

clear_locations :-
    setof(Location, X^get_location_grid_x(Location, X), Locations),
    clear_locations(Locations),
    set_shaped_positions([]),
    set_legal_positions_with_rotation([]),
    set_legal_positions([]).

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
    clear_location(Location),
    get_shaped_positions(Shaped),
    delete(Shaped, Location, TrimmedShaped),
    set_shaped_positions(TrimmedShaped).

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
    get_shaped_positions(Shaped),
    delete(Shaped, Location, OtherShaped)
      -> set_shaped_positions(OtherShaped)
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
    retract(data_locationCounter(DataID, Counter)),
    NewCounter is Counter + 1,
    asserta(data_locationCounter(DataID, NewCounter)).

get_shaped_positions(Value) :-
    data_shapedPositions(Value).

set_shaped_positions(Value) :-
    data_default_id(ID),
    retract(data_shapedPositions(ID, _)),
    asserta(data_shapedPositions(ID, Value)).

get_legal_positions(Value) :-
    data_legalPositions(Value).

set_legal_positions(Value) :-
    data_default_id(ID),
    retract(data_legalPositions(ID, _)),
    asserta(data_legalPositions(ID, Value)).

get_legal_positions_with_rotation(Value) :-
    data_legalPositionsWithRotation(Value).

set_legal_positions_with_rotation(Value) :-
    data_default_id(ID),
    retract(data_legalPositionsWithRotation(ID, _)),
    asserta(data_legalPositionsWithRotation(ID, Value)).

get_irreplaceables(Value) :-
    data_irreplaceables(Value).

set_irreplaceables(Value) :-
    data_default_id(ID),
    retract(data_irreplaceables(ID, _)),
    asserta(data_irreplaceables(ID, Value)).

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
         set_shaped_positions([ID])
    ;
     wam_duration(Start),
     candidate_spaces(Board, Candidates),
     wam_duration(Mark1),
     shaped_candidates(Candidates, ShapedLocations),
     wam_duration(Mark2),
     set_shaped_positions(ShapedLocations),
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
     (L = 2
       -> clear_locations,
          find_shaped_locations
     ;
     wam_duration(Start),
     clear_hole_inducing_shaped_locations(ID), % some old shaped locations may have become 'hole inducing' due to new Tile on board.
     incremental_candidate_spaces(ID, Candidates),
     wam_duration(Mark1),
     shaped_candidates(Candidates, NewShapedLocations),
     wam_duration(Mark2),
     get_shaped_positions(OldShapedLocations),
     append(NewShapedLocations, OldShapedLocations, ShapedLocations),
     set_shaped_positions(ShapedLocations),
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
    get_shaped_positions(ShapedLocations),
    get_tile_colors(Tile, TileColors),
    filter_shaped_for_legal_locations(ShapedLocations, TileColors, LegalWithRotation),
    filter_by_last_tile_placed(LegalWithRotation, LegalByLastTilePlaced),
    length(LegalByLastTilePlaced, LegalByLastLength),
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
    placed_position_offset(DX, DY, PlacedPositionOffset),
    ConstrainedPosition is 1 + (PlacedPositionOffset + 2) mod 4,
    get_tile_colors(PlacedTile, PlacedColors),
    wam_duration(Mark),
    nth0(PlacedPositionOffset, PlacedColors, ColorConstraint),
    set_location_constraint(ID, ConstrainedPosition, ColorConstraint),
    (get_last_build_phase_tile_placed(PlacedTile)
      -> set_location_by_last_tile_placed(ID, true)
    ;
    true
    ),
    wam_duration(End),
    !,
    display_spans([Start, Mark, End], check_orthogonal_neighbor).

placed_position_offset(DX, DY, PlacedPositionOffset) :-
    DY < 0
      -> PlacedPositionOffset = 0
    ;
    DY > 0
      -> PlacedPositionOffset = 2
    ;
    DX < 0
      -> PlacedPositionOffset = 3
    ;
    PlacedPositionOffset = 1.


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
