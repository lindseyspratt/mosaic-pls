/**
 * The tileModel function creates an object that models a tile.
 * A tile has:
 * * a unique identifier among all tiles.
 * * a gridX and gridY value to position it in the overall game grid (for both hands and the board).
 * * a sequence of color IDs.
 * * a container (either a hand or the board)
 * * an array (possibly empty) of tile identifiers for tiles that can be used to replace this tile.
 *
 * There are utility functions in the tile model object:
 * * rotate_right - rotate the tile one 'edge' to the right,
 * * rotate_left - rotate the tile one 'edge' to the left,
 * * board_hash_key - get the key identifying this tile for use in the board hash map, and
 * * edge_neighbor_tile - locate a tile that is a neighbor of this tile.
 *
 * There is a debugging function 'values' for displaying the information in the model tile object.
 * @param {Object} spec
 * @param {number} spec.tileCounter
 * @param {number[]} spec.colorIDs
 * @param {*} spec.container
 * @param {number []} [spec.replacements]
 * @param {*} [spec.minimumMismatch]
 * @param {gameModelBasics} spec.basics
 * @param {gameModelTiles} spec.model
 */

:- module(tile_model, [create_tile_model/5, save_tile_model/0, load_tile_model/0,
    save_tile_model_stream/1, retract_tile_model/0,
    get_tile_grid_x/2, get_tile_grid_y/2,
    get_tile_colors/2, get_tile_container/2,
    update_grid_x/3, update_grid_y/3, update_container/3, update_replacements/3,
    set_colors/2,
    tile_rotate_left/1, tile_rotate_right/1,
    get_tile_original_colors/2, clear_tile_original_colors/1, set_tile_original_colors/2,
    tile_board_hash_key/2, edge_neighbor_position/3]).

:- use_module('../proscriptls_sdk/library/data_predicates').
:- use_module(model_basics).
%:- use_module(game_model_tiles).

% e.g. tile_model_gridX(ID, X), tile_model_gridY(ID, Y)...
:- initialization(initdyn).

initdyn :-
    data_mode(Mode),
    data_predicate_dynamics(
        [data_predicates(tm, tile_model, [Mode],
            [gridX,gridY,colors,container,replacements,minimumMismatch,originalColors])
        ]).

save_tile_model_stream(Stream) :-
    save_data_stream(tile_model, Stream).

retract_tile_model :-
    retract_all_data(tile_model).

save_tile_model :-
    save_data(tile_model, local_storage('mosaic')).

load_tile_model :-
    load_data(tile_model, local_storage('mosaic')).

dummy_reference :-
    dummy_reference,
    tile_model_gridX(_),
    tile_model_gridY(_),
    tile_model_colors(_),
    tile_model_container(_),
    tile_model_replacements(_),
    tile_model_minimumMismatch(_).


create_tile_model(ID, GridX,GridY,Colors,Container) :-
    create_tile_model(ID, GridX,GridY,Colors,Container,[],[],none).

create_tile_model(ID, GridX,GridY,Colors,Container,Replacements,MinimumMismatch,Original) :-
    assert_data(tm(GridX,GridY,Colors,Container,Replacements,MinimumMismatch,Original), ID).

get_tile_grid_x(ID, GridX) :-
    get_tile_model_gridX(ID, GridX).

get_tile_grid_y(ID, GridY) :-
    get_tile_model_gridY(ID, GridY).

get_tile_colors(ID, Colors) :-
    ground(ID),
    get_tile_model_colors(ID, Colors),
    !.
get_tile_colors(ID, Colors) :-
    var(ID),
    get_tile_model_colors(ID, Colors).

get_tile_container(ID, Container) :-
    get_tile_model_container(ID, Container).

update_grid_x(ID, X1, X2) :-
    update_tile_model_gridX(ID, X1, X2).

update_grid_y(ID, X1, X2) :-
    update_tile_model_gridY(ID, X1, X2).

update_colors(ID, X1, X2) :-
    update_tile_model_colors(ID, X1, X2).

update_container(ID, X1, X2) :-
    update_tile_model_container(ID, X1, X2).

update_replacements(ID, X1, X2) :-
    update_tile_model_replacements(ID, X1, X2).

set_colors(ID, X) :-
    set_tile_model_colors(ID, X).

tile_rotate_left(ID) :-
    get_tile_model_colors(ID, X1),
    rotate_left(X1, X2),
    update_colors(ID, X1, X2).

tile_rotate_right(ID) :-
    get_tile_model_colors(ID, X1),
    rotate_left(X1, X2),
    update_colors(ID, X1, X2).

get_tile_original_colors(ID, Colors) :-
    get_tile_model_originalColors(ID, Colors).

set_tile_original_colors(ID, Colors) :-
    set_tile_model_originalColors(ID, Colors).

clear_tile_original_colors(ID) :-
    clear_tile_model_originalColors(ID).

tile_board_hash_key(ID, Key) :-
    get_tile_model_gridX(ID, X),
    get_tile_model_gridY(ID, Y),
    board_hash_key_coords(X, Y, Key).

edge_neighbor_position(ID, Edge, X > Y) :-
    edge_neighbor_offset(Edge, PX > PY),
    get_tile_grid_x(ID, GridX),
    get_tile_grid_y(ID, GridY),
    X is GridX + PX,
    Y is GridY + PY.
