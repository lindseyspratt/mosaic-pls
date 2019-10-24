/**
 * The gameModelTiles object function returns an object that describes the current state of the game.
 * This includes the tiles used in the game, the hands and board uses of those tiles, a record of
 * the last 2 tiles placed, the current turn, and the current game phase.
 * @param {Object} spec
 * @param {gameModelBasics} spec.gameModelBasics
 */


:- module(game_model_tiles,
    [init_game_model_tiles/0, get_tiles/1, get_total_tiles_in_game/1, get_board/1, get_hands/1, get_hand/2,
     get_turn/1, increment_turn/1, get_selected_tile_id/1, set_selected_tile_id/1,
     get_replacements/1, set_replacements/1, get_board_tile_by_grid/3, set_last_build_phase_tile_placed/1,
     last_placed_tiles/2, place_tile_in_hand/1, place_tile_on_board/3, get_game_phase/1,
     update_game_phase/0, edge_neighbor_tile/3, game_model_tiles_values/1]).

:- use_module('../proscriptls_sdk/library/data_predicates').
:- use_module(model_basics).
:- use_module(tile_model).
:- use_module(view_basics).
:- use_module(library).

:- initialization(initdyn).

initdyn :-
  data_predicate_dynamics([
      data_predicates(gmt, data,[tile_counter, tiles, hands, board,
          tilesPlaced, boardHash,
          lastPlacedTile1, lastPlacedTile2, lastBuildPhaseTilePlacedID,
          turn, selectedTileID, replacements, gamePhase])]).

init_game_model_tiles :-
    assert_data(gmt(0, [], [], [], 0, [], none, none, none, 0, none, [], none), 1),
    build_tiles.

make_hand_tile(PlayerID, Colors, NewTileID) :-
    increment_tile_counter(NewTileID),
    create_tile_model(NewTileID, 0,0,Colors,hand(PlayerID)).

%make_hand_tiles(HandColors, PlayerID, HandTiles).
make_hand_tiles([], _PlayerID, []).
make_hand_tiles([HC|TC], PlayerID, [HT|TT]) :-
   make_hand_tile(PlayerID, HC, HT),
   make_hand_tiles(TC, PlayerID, TT).

build_tiles :-
    set_tile_counter(0),
    get_hand_color_ids(HandColors),
    build_tiles(HandColors, 1, HandTiles, AllTileIDs),
    set_hands(HandTiles),
    set_tiles(AllTileIDs).

build_tiles([], _PlayerID, [], []).
build_tiles([H|T], PlayerID, [HT|TT], AllTileIDs) :-
    make_hand_tiles(H, PlayerID, HT),
    append(HT, OtherTileIDs, AllTileIDs),
    NextPlayerID is PlayerID + 1,
    build_tiles(T, NextPlayerID, TT, OtherTileIDs).


increment_tile_counter(NewCounter) :-
    data_default_id(GameModelTilesID),
    retract(data_tile_counter(GameModelTilesID, OldCounter)),
    NewCounter is OldCounter + 1,
    asserta(data_tile_counter(GameModelTilesID, NewCounter)).

set_tile_counter(Counter) :-
    data_default_id(GameModelTilesID),
    retractall(data_tile_counter(GameModelTilesID, _)),
    asserta(data_tile_counter(GameModelTilesID, Counter)).

set_hands(HandTiles) :-
    data_default_id(GameModelTilesID),
    retractall(data_hands(GameModelTilesID, _)),
    asserta(data_hands(GameModelTilesID, HandTiles)).

set_tiles(Tiles) :-
    data_default_id(GameModelTilesID),
    retractall(data_tiles(GameModelTilesID, _)),
    asserta(data_tiles(GameModelTilesID, Tiles)).

get_tiles(T) :- data_tiles(T).

get_total_tiles_in_game(Count) :- data_tile_counter(Count).

get_board(Board) :- data_board(Board).

get_hands(Hands) :- data_hands(Hands).

get_hand(PlayerID, Hand) :-
    data_hands(Hands),
    nth1(PlayerID, Hands, Hand).

add_hand_tile(PlayerID, Tile) :-
    data_default_id(ID),
    retractall(data_hands(ID, Hands)),
    add_hand_tile(Hands, PlayerID, Tile, NewHands),
    asserta(data_hands(ID, NewHands)).

add_hand_tile([H|T], 1, Tile, [[Tile|H]|T]) :-
    !.
add_hand_tile([H|T], IDCounter, Tile, [H|TN]) :-
    NextIDCounter is IDCounter - 1,
    add_hand_tile(T, NextIDCounter, Tile, TN).

get_turn(Turn) :- data_turn(Turn).

increment_turn(NewTurn) :-
    data_default_id(GameModelTilesID),
    retract(data_turn(GameModelTilesID, OldTurn)),
    get_number_of_players(NumberOfPlayers),
    NewTurn is (OldTurn mod NumberOfPlayers) + 1,
    asserta(data_turn(GameModelTilesID, NewTurn)).

get_selected_tile_id(Selected) :-
    data_selectedTileID(Selected).

set_selected_tile_id(Selected) :-
    data_default_id(GameModelTilesID),
    retractall(data_selectedTileID(GameModelTilesID, _)),
    asserta(data_selectedTileID(GameModelTilesID, Selected)).

get_replacements(Replacements) :-
    data_replacements(Replacements).

set_replacements(Replacements) :-
    data_default_id(GameModelTilesID),
    retractall(data_replacements(GameModelTilesID, _)),
    asserta(data_replacements(GameModelTilesID, Replacements)).

get_board_tile_by_grid(GridX, GridY, TileID) :-
    board_hash_key_coords(GridX, GridY, Key),
    data_boardHash(BoardHash),
    member(Key-TileID, BoardHash).

set_last_build_phase_tile_placed(ID) :-
    data_default_id(GameModelTilesID),
    retractall(data_slastBuildPhaseTilePlacedID(GameModelTilesID, _)),
    asserta(data_lastBuildPhaseTilePlacedID(GameModelTilesID, ID)).

last_placed_tiles(Tile1, Tile2) :-
    get_tile_grid_x(Tile1, GridX1),
    get_tile_grid_y(Tile1, GridY1),
    get_tile_grid_x(Tile2, GridX2),
    get_tile_grid_y(Tile2, GridY2),
    ((GridX1 > GridX2
     ;
     GridX1 = GridX2,
     GridY1 > GridY2
    ) -> SortedTile1 = Tile2,
         SortedTile2 = Tile1
    ;
    SortedTile1 = Tile1,
    SortedTile2 = Tile2
    ),
    data_lastPlacedTile1(SortedTile1),
    data_lastPlacedTile2(SortedTile2).

remove_tile_from_board_hash(Tile) :-
    tile_board_hash_key(Tile, Key),
    data_default_id(GameModelTilesID),
    retract(data_boardHash(GameModelTilesID, BoardHash)),
    delete(BoardHash, Key-Tile, RemainderBoardHash),
    asserta(data_boardHash(GameModelTilesID, RemainderBoardHash)).

remove_tile_from_container(Tile) :-
    get_tile_container(Tile, Container),
    (Container = hand(PlayerID)
      -> data_default_id(GameModelTilesID),
         retract(data_hands(GameModelTilesID, Hands)),
         nth1(PlayerID, Hands, Hand),
         delete(Hand, Tile, RemainingHand),
         asserta(data_hands(GameModelTilesID, RemainingHand))
    ;
     Container = board
      -> data_default_id(GameModelTilesID),
         retract(data_tilesOnBoard(GameModelTilesID, Board)),
         delete(Board, Tile, RemainingBoard),
         asserta(data_tilesOnBoard(GameModelTilesID, RemainingBoard)),
         remove_tile_from_board_hash(Tile)
    ;
     throw(mosaic_internal('invalid container type', Container))
    ).

place_tile_in_hand(Tile) :-
    remove_tile_from_container(Tile),
    update_grid_x(Tile, _, 0),
    update_grid_y(Tile, _, 0),
    data_turn(Turn),
    update_container(Tile, _, hand(Turn)),
    add_hand_tile(Turn, Tile).

place_tile_on_board(Tile, GridX, GridY) :-
    get_view_tile(Tile, ViewTile),
    get_tile_container(Tile, Container),
    (Container = hand(_PlayerID)
      -> update_grid_x(Tile, _, GridX),
         update_grid_y(Tile, _, GridY),
         add_board_hash_tile(Tile),
         remove_tile_from_container(Tile),
         update_container(Tile, _, board),
         get_game_phase(Phase),
         (Phase = build
           -> set_lastBuildPhaseTilePlacedID(Tile)
          ;
          set_lastBuildPhaseTilePlacedID(none)
         )
    ;
     Container = board
      -> remove_tile_from_board_hash(Tile),
         update_grid_x(Tile, _, GridX),
         update_grid_y(Tile, _, GridY),
         add_board_hash_tile(Tile)
    ;
     throw(mosaic_internal('invalid container type (place_tile_on_board)', Container))
    ),
    board_tile_size(Size),
    set_view_tile_size(ViewTile, Size),
    get_top_left_board_tile_coords(GridX, GridY, ViewX, ViewY),
    set_view_tile_grid_x(ViewTile, ViewX),
    set_view_tile_grid_y(ViewTile, ViewY).

get_game_phase(Phase) :-
    data_gamePhase(Phase).

update_game_phase :-
    data_gamePhase(build),
    data_board(Board),
    length(Board, BoardLength),
    data_tiles(Tiles),
    length(Tiles, TilesLength),
    BoardLength = TilesLength
      -> data_default_id(GameModelTilesID),
         retract(data_gamePhase(GameModelTilesID, _)),
         asserta(data_board_gamePhase(GameModelTilesID, transform))
    ;
    true.

edge_neighbor_tile(ID, Edge, NeighborID) :-
    edge_neighbor_position(ID, Edge, Position),
    get_board_tile_by_grid(Position, NeighborID).

game_model_tiles_values(Values) :-
    data_default_id(ID),
    labelled_values(data, ID, Values).