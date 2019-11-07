:- module(location_model,
    [create_location_model/3, clear_location/1, get_location_grid_x/2, get_location_grid_y/2,get_location_neighbors/2, get_location_orthogonal_neighbors/2,
     get_location_by_last_tile_placed/2, set_location_by_last_tile_placed/2,
     get_location_constraints/2, set_location_constraints/2, set_location_constraint/3, get_location_forced_colors/2, set_location_forced_color/3,
     get_location_replacements/2, get_location_minimum_mismatch/2, increment_location_neighbors/2, increment_location_orthogonal_neighbors/2,
     location_edge_neighbor_position/3, location_edge_second_neighbor_position/3, location_model_values/2]).

:- use_module('../proscriptls_sdk/library/data_predicates').
:- use_module(tile_model).
:- use_module(model_basics).
:- use_module(game_view_tiles).

:- initialization(initdyn).

initdyn :-
    data_predicate_dynamics([data_predicates(lm, data,[gridX,gridY,neighbors,orthogonalNeighbors,byLastTilePlaced,constraints,forcedColors,replacements,minimumMismatch])]).

dummy_reference :-
    dummy_reference,
    data_gridX(_),
    data_gridY(_),
    data_neighbors(_),
    data_orthogonalNeighbors(_),
    data_byLastTilePlaced(_),
    data_constraints(_),
    data_forcedColors(_),
    data_replacements(_),
    data_minimumMismatch(_).

create_location_model(ID, GridX, GridY) :-
    assert_data(lm(GridX, GridY, 0, 0, false, [-1,-1,-1,-1], [], [], []), ID).

clear_location(ID) :-
    retract_data(data, ID).

get_location_grid_x(ID, Value) :-
    data_gridX(ID, Value).

get_location_grid_y(ID, Value) :-
    data_gridY(ID, Value).

get_location_neighbors(ID, Value) :-
    data_neighbors(ID, Value).

get_location_orthogonal_neighbors(ID, Value) :-
    data_orthogonalNeighbors(ID, Value).

get_location_by_last_tile_placed(ID, Value) :-
    data_byLastTilePlaced(ID, Value).

set_location_by_last_tile_placed(ID, Value) :-
    retract(data_byLastTilePlaced(ID, _)),
    asserta(data_byLastTilePlaced(ID, Value)).

get_location_constraints(ID, Value) :-
    data_constraints(ID, Value).

set_location_constraints(ID, Value) :-
    retract(data_constraints(ID, _)),
    asserta(data_constraints(ID, Value)).

set_location_constraint(ID, Position, Value) :-
    retract(data_constraints(ID, Old)),
    replace(Old, Position, Value, New),
    asserta(data_constraints(ID, New)).

get_location_forced_colors(ID, Value) :-
    data_forcedColors(ID, Value).

set_location_forced_color(ID, Position, Value) :-
    retract(data_forcedColors(ID, Old)),
    replace(Old, Position, Value, New),
    asserta(data_forcedColors(ID, New)).

get_location_replacements(ID, Value) :-
    data_replacements(ID, Value).

get_location_minimum_mismatch(ID, Value) :-
    data_minimumMismatch(ID, Value).

increment_location_neighbors(ID, New) :-
    retract(data_neighbors(ID, Old)),
    New is Old + 1,
    asserta(data_neighbors(ID, New)).

increment_location_orthogonal_neighbors(ID, New) :-
    retract(data_orthogonalNeighbors(ID, Old)),
    New is Old + 1,
    asserta(data_orthogonalNeighbors(ID, New)).

replace([_|T], 1, Value, [Value|T]) :-
    !.
replace([H|T], Position, Value, [H|NT]) :-
    Position > 1,
    Next is Position - 1,
    replace(T, Next, Value, NT).

location_edge_neighbor_position(ID, Edge, TX > TY) :-
    edge_neighbor_offset(Edge, DX > DY),
    data_gridX(ID, GridX),
    data_gridY(ID, GridY),
    TX is GridX + DX,
    TY is GridY + DY.

%location_edge_neighbor_tile(ID, Edge, NeighborTile) :-
%    location_edge_neighbor_position(ID, Edge, TX > TY),
%    get_board_tile_by_grid(TX > TY, NeighborTile).

location_edge_second_neighbor_position(ID, Edge, TX > TY) :-
    edge_neighbor_offset(Edge, DX > DY),
    data_gridX(ID, GridX),
    data_gridY(ID, GridY),
    TX is GridX + 2*DX,
    TY is GridY + 2*DY.

location_model_values(ID, Values) :-
    labelled_values(data, ID, Values).
