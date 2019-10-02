/**
 * The gameModelTiles object function returns an object that describes the current state of the game.
 * This includes the tiles used in the game, the hands and board uses of those tiles, a record of
 * the last 2 tiles placed, the current turn, and the current game phase.
 * @param {Object} spec
 * @param {gameModelBasics} spec.gameModelBasics
 */


:- module(game_model_tiles, [game_model_tiles_values/1]).

:- use_module('../proscriptls_sdk/library/data_predicates').
:- use_module(model_basics).
:- use_module(tile_model).
:- use_module(view_basics).
:- use_module(library).

:- initialization(initdyn).

initdyn :-
  data_predicate_dynamics([
      data_predicates(gmt, game_model_tiles,[tile_counter, tiles, hands, board,
          tilesPlaced, boardHash,
          lastPlacedTile1, lastPlacedTile2, lastBuildPhaseTilePlacedID,
          turn, selectedTileID, replacements, gamePhase])]).

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
    set_tiles(ALLTileIDs).

build_tiles([], _PlayerID, [], []).
build_tiles([H|T], PlayerID, [HT|TT], AllTileIDs) :-
    make_hand_tiles(H, PlayerID, HT),
    append(HT, OtherTileIDs, AllTileIDs),
    build_tiles(T, PlayerID, TT, OtherTileIDs).


increment_tile_counter(NewCounter) :-
    game_model_tiles_default_id(GameModelTilesID),
    retract(game_model_tiles_tile_counter(GameModelTilesID, OldCounter)),
    NewCounter is OldCounter + 1,
    asserta(game_model_tiles_tile_counter(GameModelTilesID, NewCounter)).

set_tile_counter(Counter) :-
    game_model_tiles_default_id(GameModelTilesID),
    retractall(game_model_tiles_tile_counter(GameModelTilesID, _)),
    asserta(game_model_tiles_tile_counter(GameModelTilesID, Counter)).

set_hands(HandTiles) :-
    game_model_tiles_default_id(GameModelTilesID),
    retractall(game_model_tiles_hands(GameModelTilesID, _)),
    asserta(game_model_tiles_hands(GameModelTilesID, HandTiles)).

set_tiles(Tiles) :-
    game_model_tiles_default_id(GameModelTilesID),
    retractall(game_model_tiles_tiles(GameModelTilesID, _)),
    asserta(game_model_tiles_tiles(GameModelTilesID, Tiles)).

get_tiles(T) :- game_model_tiles_tiles(T).

get_total_tiles_in_game(Count) :- game_model_tiles_tile_counter(Count).

get_board(Board) :- game_model_tiles_board(Board).

get_hands(Hands) :- game_model_tiles_hands(Hands).

get_hand(PlayerID, Hand) :-
    game_model_tiles_hands(Hands),
    nth1(PlayerID, Hands, Hand).

add_hand_tile(PlayerID, Tile) :-
    game_model_tiles_default_id(ID),
    retractall(game_model_tiles_hands(ID, Hands)),
    add_hand_tile(Hands, PlayerID, Tile, NewHands),
    asserta(game_model_tiles_hands(ID, NewHands)).

add_hand_tile([H|T], 1, Tile, [[Tile|H]|T]) :-
    !.
add_hand_tile([H|T], IDCounter, Tile, [H|TN]) :-
    NextIDCounter is IDCounter - 1,
    add_hand_tile(T, NextIDCounter, Tile, TN).

get_turn(Turn) :- game_model_tiles_turn(Turn).

increment_turn(NewTurn) :-
    game_model_tiles_default_id(GameModelTilesID),
    retract(game_model_tiles_turn(GameModelTilesID, OldTurn)),
    NewTurn is (OldTurn mod NumberOfPlayers) + 1,
    asserta(game_model_tiles_turn(GameModelTilesID, NewTurn)).

get_selected_tile_id(Selected) :-
    game_model_tiles_selectedTileID(Selected).

set_selected_tile_id(Selected) :-
    game_model_tiles_default_id(GameModelTilesID),
    retractall(game_model_tiles_selectedTileID(GameModelTilesID, _)),
    asserta(game_model_tiles_selectedTileID(GameModelTilesID, Selected)).

get_replacements(Replacements) :-
    game_model_tiles_replacements(Replacements).

set_replacements(Replacements) :-
    game_model_tiles_default_id(GameModelTilesID),
    retractall(game_model_tiles_replacements(GameModelTilesID, _)),
    asserta(game_model_tiles_replacements(GameModelTilesID, Replacements)).

get_board_tile_by_grid(GridX, GridY, TileID) :-
    board_hash_key_coords(GridX, GridY, Key),
    game_model_tiles_boardHash(BoardHash),
    member(Key-TileID, BoardHash).

set_lastBuildPhaseTilePlacedID(ID) :-
    game_model_tiles_default_id(GameModelTilesID),
    retractall(game_model_tiles_slastBuildPhaseTilePlacedID(GameModelTilesID, _)),
    asserta(game_model_tiles_lastBuildPhaseTilePlacedID(GameModelTilesID, ID)).

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
    game_model_tiles_lastPlacedTile1(SortedTile1),
    game_model_tiles_lastPlacedTile2(SortedTile2).

remove_tile_from_board_hash(Tile) :-
    tile_board_hash_key(Tile, Key),
    game_model_tiles_default_id(GameModelTilesID),
    retract(game_model_tiles_boardHash(GameModelTilesID, BoardHash)),
    delete(BoardHash, Key-Tile, RemainderBoardHash),
    asserta(game_model_tiles_boardHash(GameModelTilesID, RemainderBoardHash)).

remove_tile_from_container(Tile) :-
    get_tile_container(Tile, Container),
    (Container = hand(PlayerID)
      -> game_model_tiles_default_id(GameModelTilesID),
         retract(game_model_tiles_hands(GameModelTilesID, Hands)),
         nth1(PlayerID, Hands, Hand).
         delete(Hand, Tile, RemainingHand),
         asserta(game_model_tiles_hands(GameModelTilesID, RemainingHand))
    ;
     Container = board
      -> game_model_tiles_default_id(GameModelTilesID),
         retract(game_model_tiles_tilesOnBoard(GameModelTilesID, Board)),
         delete(Board, Tile, RemainingBoard),
         asserta(game_model_tiles_tilesOnBoard(GameModelTilesID, RemainingBoard)),
         remove_tile_from_board_hash(Tile)
    ;
     throw(mosaic_internal('invalid container type', Container))
    ).

place_tile_in_hand(Tile) :-
    remove_tile_from_container(Tile),
    update_grid_x(Tile, _, 0),
    update_grid_y(Tile, _, 0),
    game_model_tiles_turn(Turn),
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
    game_model_tiles_gamePhase(Phase).


update_game_phase :-
    game_model_tiles_gamePhase(build),
    game_model_tiles_board(Board),
    length(Board, BoardLength),
    game_model_tiles_tiles(Tiles),
    length(Tiles, TilesLength),
    BoardLength = TilesLength
      -> game_model_tiles_default_id(GameModelTilesID),
         retract(game_model_tiles_gamePhase(GameModelTilesID, _)),
         asserta(game_model_tiles_board_gamePhase(GameModelTilesID, transform))
    ;
    true.

game_model_tiles_values([tiles-Tiles,board-Board,hands-Hands,turn-Turn,selected-Selected,phase-Phase,replacements-Replacements]) :-
    game_model_tiles_tiles(Tiles),
    game_model_tiles_board(Board),
    game_model_tiles_turn(Turn),
    game_model_tiles_selectedTileID(Selected),
    game_model_tiles_gamePhase(Phase),
    game_model_tiles_replacements(Replacements).


game_model_tiles_values(Values) :-
    game_model_tiles_default_id(ID),
    labelled_values(game_model_tiles, ID, Values).
