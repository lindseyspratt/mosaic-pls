:- module(locations, [create_locations/0, clear_locations/0,
    get_legal_positions/1, push_legal_position/1, set_legal_positions/1, get_legal_positions_with_rotation/1,
    set_legal_positions_with_rotation/1, get_irreplaceables/1, set_irreplaceables/1, update_legal_positions/1, set_possible_build_positions/0 ]).

:- use_module('../proscriptls_sdk/library/data_predicates').
:- use_module(location_model).
:- use_module(model_basics).
:- use_module(tile_model).
:- use_module(game_model_tiles).
:- use_module(library). % nth0/3

:- initialization(initdyn).

initdyn :-
    data_predicate_dynamics([data_predicates(loc, data,[locationCounter, legalPositions,legalPositionsWithRotation,irreplaceables])]).

create_locations :-
    assert_data(loc(0, [], [], []), 1).

clear_locations :-
    data_legalPositionsWithRotation(Locations),
    clear_locations(Locations).

clear_locations([]).
clear_locations([H|T]) :-
    clear_location(H),
    clear_locations(T).

increment_location_counter(NewCounter) :-
    data_default_id(DataID),
    retract(data_locationCounter(DataID, Counter)),
    NewCounter is Counter + 1,
    asserta(data_locationCounter(DataID, NewCounter)).

get_legal_positions(Value) :-
    data_legalPositions(Value).

push_legal_position(Value) :-
    data_default_id(ID),
    retract(data_legalPositions(ID, Positions)),
    asserta(data_legalPositions(ID, [Value|Positions])).

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
    update_legal_positions(PositionsWithRotations, Colors, ConstrainedPositions),
    set_legal_positions(ConstrainedPositions).

update_legal_positions([], _Colors, []).
update_legal_positions([H|T], Colors, Constrained) :-
    update_legal_position(H, Colors, Constrained, NextConstrained),
    update_legal_positions(T, Colors, NextConstrained).

update_legal_position(H, Colors, Constrained, ConstrainedTail) :-
    get_location_constraints(H, Constraints),
    (tile_colors_match_constraint_colors(Colors, Constraints)
      -> Constrained = [H|ConstrainedTail]
    ;
    Constrained = ConstrainedTail
    ).

set_possible_build_positions :-
    get_board(Board),
    (Board = []
      -> create_location(ID, 0, 0),
         set_legal_positions_with_rotation([ID])
    ;
     candidate_spaces(Board, Candidates),
     legal_candidates(Candidates, LegalWithRotation, LegalByLastTilePlaced),
     length(LegalByLastTilePlaced, LegalByLastLength),
     (LegalByLastLength > 0
        -> FinalLegalWithRotation = LegalByLastTilePlaced
     ;
     FinalLegalWithRotation = LegalWithRotation
     ),
     set_legal_positions_with_rotation(FinalLegalWithRotation)
    ).

candidate_spaces(Board, Candidates) :-
    candidate_spaces(Board, [], Candidates).

candidate_spaces([], Candidates, Candidates).
candidate_spaces([H|T], CandidatesSoFar, CandidatesTotal) :-
    candidate_space(H, CandidatesSoFar, CandidatesNext),
    candidate_spaces(T, CandidatesNext, CandidatesTotal).

candidate_space(PlacedTile, CandidatesIn, CandidatesOut) :-
    check_neighbors1([-1,0,1], PlacedTile, CandidatesIn, CandidatesOut).

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
    (\+ get_board_tile_by_grid(TX, TY, _NeighborTile)
      -> check_neighbor(PlacedTile, TX, TY, DX, DY, CandidatesIn, CandidatesOut)
    ;
    CandidatesIn = CandidatesOut
    ).

check_neighbor(PlacedTile, TX, TY, DX, DY, CandidatesIn, CandidatesOut) :-
    board_hash_key_coords(TX, TY, Key),
    (member(Key-ID, CandidatesIn)
      -> CandidatesIn = CandidatesOut
    ;
     create_location(ID, TX, TY),
     [Key-ID|CandidatesIn] = CandidatesOut
    ),
    increment_location_neighbors(ID),
    ((DX = 0; DY = 0)
      -> check_orthogonal_neighbor(PlacedTile, DX, DY, ID)
    ;
     true
    ).

check_orthogonal_neighbor(PlacedTile, DX, DY, ID) :-
    increment_location_orthogonal_neighbors(ID),
    placed_position_offset(DX, DY, PlacedPositionOffset),
    ConstrainedPosition is 1 + (PlacedPositionOffset + 2) mod 4,
    get_tile_colors(PlacedTile, PlacedColors),
    nth0(PlacedPositionOffset, PlacedColors, ColorConstraint),
    set_location_constraint(ID, ConstrainedPosition, ColorConstraint),
    (get_last_tile_placed_id(PlacedTile)
      -> set_by_last_tile_placed(ID, true)
    ;
    true
    ).

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


legal_candidates([], [], []).
legal_candidates([H|T], LegalWithRotation, LegalByLastTilePlaced) :-
    legal_candidate(H, LegalWithRotation, LegalTail, LegalByLastTilePlaced, LegalByLastTilePlacedTail),
    legal_candidates(T, LegalTail, LegalByLastTilePlacedTail).

legal_candidate(Candidate, LegalWithRotation, LegalTail, LegalByLastTilePlaced, LegalByLastTilePlacedTail) :-
    get_location_constraints(Candidate, Constraints),
    (Constraints = []
      -> LegalWithRotation = LegalTail,
         LegalByLastTilePlaced = LegalByLastTilePlacedTail
    ;
    get_location_neighbors(Candidate, Neighbors),
    get_board(Board),
    length(Board, Length),
    Neighbors < min(2, Length)
      -> LegalWithRotation = LegalTail,
         LegalByLastTilePlaced = LegalByLastTilePlacedTail
    ;
    candidate_would_create_hole(Constraints, Candidate)
      -> LegalWithRotation = LegalTail,
         LegalByLastTilePlaced = LegalByLastTilePlacedTail
    ;
    matching_rotations(Candidate, LegalWithRotation, LegalTail)
      -> LegalWithRotation = [Candidate|LegalTail],
         (get_location_by_last_tile_placed(Candidate, true)
           -> LegalByLastTilePlaced = [Candidate|LegalByLastTilePlacedTail]
         ;
         LegalByLastTilePlaced = LegalByLastTilePlacedTail
         )
    ;
    true
      -> LegalWithRotation = LegalTail,
         LegalByLastTilePlaced = LegalByLastTilePlacedTail
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

/*
mosaic_locations.gameModelLocations = function(spec) {
	var that = {};

	var legalPositions = [];
	var legalPositionsWithRotation = [];
    var irreplaceables = [];

	var get_legal_positions = function() {
		return legalPositions;
	}

	var push_legal_position = function(value) {
		legalPositions.push(value);
	}

	var set_legal_positions = function(value) {
		legalPositions = value;
	}

	var get_legal_positions_with_rotation = function() {
		return legalPositionsWithRotation;
	}

	var set_legal_positions_with_rotation = function(value) {
		legalPositionsWithRotation = value;
	}

	var get_irreplaceables = function() {
		return irreplaceables;
	}

	var set_irreplaceables = function(value) {
		irreplaceables = value;
	}

	var update_legal_positions = function(colors) {
		legalPositions = legalPositionsWithRotation.slice(); // get copy
	    for (var i = 0 ; i < legalPositions.length; i++) {
	        if (!spec.gameModelBasics.tile_colors_match_constraint_colors(colors, legalPositions[i].constraints)) {
	            legalPositions.splice(i--, 1);
	        }
	    }
	}

	var set_possible_build_positions = function(tile) {
	    var candidateSpaces = {};
	    var board = spec.gameModelTiles.get_board();

	    for (var i = 0; i < board.length; i++) {
	        var placedTileID = board[i];
	        var placedTile = spec.gameModelTiles.get_tile(placedTileID);

	        for (var dy = -1; dy <= 1; dy++) {
	            for (var dx = -1; dx <= 1; dx++) {
	                var tx = placedTile.get_grid_x() + dx;
	                var ty = placedTile.get_grid_y() + dy;
	                var tileByGrid = spec.gameModelTiles.get_board_tile_by_grid(tx, ty);

	                if (!ifdefor(tileByGrid)) {
	                	var key = spec.gameModelBasics.board_hash_key_coords(tx, ty);
	                    var candidateSpace = ifdefor(candidateSpaces[key], locationModel({'gridX': tx, 'gridY': ty}));
	                    candidateSpace.increment_neighbors();
	                    if (dx === 0 || dy === 0) {
	                        var placedPositionOffset = (dy < 0) ? 0
	                                                         : ((dy > 0) ? 2
	                                                                     : ((dx < 0) ? 3 : 1));
	                        var constrainedPositionOffset = (placedPositionOffset + 2) % 4;
	                        var colorOffset = placedTile.colors[placedPositionOffset];
	                        candidateSpace.set_constraints(ifdefor(candidateSpace.get_constraints(), [-1,-1,-1,-1]));
	                        candidateSpace.set_constraint(constrainedPositionOffset, colorOffset);
	                        candidateSpace.set_by_last_tile_placed(ifdefor(candidateSpace.get_by_last_tile_placed(), false) || spec.gameModelTiles.get_last_tile_placed_id() === placedTile,get_id());
	                    }
	                    candidateSpaces[key] = candidateSpace;
	                }
	            }
	        }
	    }

//	    console.log("findLegalPositions: candidateSpaces");
//	    console.log(candidateSpaces);

	    var legalPositions = [];
	    var legalPositionsByLastTilePlaced = [];
	    $.each(candidateSpaces, function (hashKey, candidateSpace) {
	        var colorConstraints = candidateSpace.constraints;
	        if (!colorConstraints || candidateSpace.neighbors < Math.min(2, board.length)) {
	            return;
	        }

	        // check compactness
	    	var spaceWouldCreateHole = false;
	    	for(var constraintOfst = 0;constraintOfst < colorConstraints.length;constraintOfst++) {
	    		var constraintColor = colorConstraints[constraintOfst];
	    		if(constraintColor === -1) {
	    			// -1 implies there is no tile on edge/constraint at constraintOfst.
	    			// Check if there is a tile the next space beyond.
	    			// If there is, then placing in this location would create
	    			// a shape violation 'hole' in the board.
	    			var firstNeighborPosition = candidateSpace.edge_neighbor_position(constraintOfst);
//	    			console.log("firstNeighborPosition:");
//	    			console.log(firstNeighborPosition);

	    			var secondNeighborTile = firstNeighborPosition.edge_neighbor_tile(
	    					{"edge":constraintOfst, "model":spec.gameModelTiles});

	    			if(secondNeighborTile != null) {
	    				spaceWouldCreateHole = true;
//	    				console.log("SpaceWouldCreateHole: [candidate, firstNeighborPosition, secondNeighborTile] " );
//	    				console.log([candidateSpace,firstNeighborPosition,secondNeighborTile]);
	    				break;
	    			}
	    		}
	    	}

//	    	console.log("findLegalPositions: spaceWouldCreateHole " + spaceWouldCreateHole);
//	    	console.log(candidateSpace);

	    	if(spaceWouldCreateHole) {
	            return;
	    	}

	        // copy so we can rotate without changing the original.
	        var rotatedColors = tile.colors.slice();
	        for (var rotationAttempts = 0; rotationAttempts < 4; rotationAttempts++) {
	            if (tileColorsMatchConstraintColors(rotatedColors, colorConstraints)) {
	                legalPositions.push(candidateSpace);
	                if (candidateSpace.get_by_last_tile_placed()) {
	                    legalPositionsByLastTilePlaced.push(candidateSpace);
	                }
	                break;
	            }
	            rotatedColors = rotateRight(rotatedColors);
	        }
	    });
	    legalPositionsWithRotation = legalPositionsByLastTilePlaced.length ? legalPositionsByLastTilePlaced : legalPositions;
	}

	var values = function() {
		var display = {};

		display.get_legal_positions = get_legal_positions();
		display.get_legal_positions_with_rotation = get_legal_positions_with_rotation();
		display.get_irreplaceables=get_irreplaceables();

		return display;
	}

	that.get_legal_positions = get_legal_positions;
	that.push_legal_position = push_legal_position;
	that.set_legal_positions = set_legal_positions;
	that.get_legal_positions_with_rotation = get_legal_positions_with_rotation;
	that.set_legal_positions_with_rotation = set_legal_positions_with_rotation;
	that.get_irreplaceables = get_irreplaceables;
	that.set_irreplaceables = set_irreplaceables;

	that.update_legal_positions = update_legal_positions;

	that.values = values;

	return that;
}
*/