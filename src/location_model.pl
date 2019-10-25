:- module(location_model,
    [get_location_grid_x/2, get_location_grid_y/2,get_location_neighbors/2, get_location_orthogonal_neighbors/2, get_location_by_last_tile_placed/2,
     get_location_constraints/2, set_location_constraints/2, set_location_constraint/3, get_location_forced_colors/2, set_location_forced_color/3,
     get_location_replacements.2, get_location_minimum_mismatch/2, increment_location_neighbors/2, increment_location_orthogonal_neighbors/2,
     location_edge_neighbor_position/4, location_edge_neighbor_tile/3, point_in_location/3, location_model_values/2]).

:- initialization(initdyn).

initdyn :-
    data_predicate_dynamics([data_predicates(lm, data,[gridX,gridY,neighbors,orthogonalNeighbors,byLastTilePlaced,constraints,forcedColors,replacements,minimumMismatch])]).


get_location_grid_x(ID, Value) :-
    data_gridX(ID, Value).

get_location_grid_y(ID, Value) :-
    data_gridY(ID, Value).

get_location_neighbors(ID, Value) :-
    data_neighbors(ID, Value).

get_location_orthogonal_neighbors(ID, Value) :-
    data_orthogonalNeighbors(ID, Value).

get_location_by_last_tile_placed(ID, Value) :-
    data_getByLastTilePlaced(ID, Value).

get_location_constraints(ID, Value) :-
    data_constraints(ID, Value).

set_location_constraints(ID, Value) :-
    retracta(data_constraints(ID, _)),
    asserta(data_constraints(ID, Value)).

set_location_constraint(ID, Position, Value) :-
    retracta(data_constraints(ID, Old)),
    replace(Old, Position, Value, New),
    asserta(data_constraints(ID, New)).

get_location_forced_colors(ID, Value) :-
    data_forcedColors(ID, Value).

set_location_forced_color(ID, Position, Value) :-
    retracta(data_forcedColors(ID, Old)),
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
    retract(data_northogonalNeighbors(ID, Old)),
    New is Old + 1,
    asserta(data_orthogonalNeighbors(ID, New)).

replace([H|T], 1, Value, [Value|T]) :-
    !.
replace([H|T], Position, Value, [H|NT]) :-
    Position > 1,
    Next is Position - 1,
    replace(T, Next, Value, NT).

location_edge_neighbor_position(ID, Edge, TX, TY) :-
    edge_neighbor_offset(Edge, DX > DY),
    data_gridX(ID, GridX),
    data_gridY(ID, GridY),
    TX is GridX + DX,
    TY is GridY + DY.

location_edge_neighbor_tile(ID, Edge, NeighborTile) :-
    location_edge_neighbor_position(ID, Edge, TX, TY),
    get_board_tile_by_grid(TX, TY, NeighborTile).

point_in_location(ID, X, Y) :-
    data_gridX(ID, GridX),
    data_gridY(ID, GridY),
    point_in_board_position(GridX, GridY, X, Y).

location_model_values(ID, Values) :-
    labelled_values(data, ID, Values).
