/**
 * The gameModelTiles object function returns an object that describes the current state of the game.
 * This includes the tiles used in the game, the hands and board uses of those tiles, a record of
 * the last 2 tiles placed, the current turn, and the current game phase.
 *
 * The game phase names the next action to be taken by the player identified by the current turn.
 * build = build the board by adding a tile to it. Adding a tile has subactions:
 *      select/change hand tile,
 *      rotate selected tile,
 *      move hand tile to board by selecting board location.
 * transform = transform the board by selecting a shared edge between two board tiles.
 * replace = replace a tile selected for transformation (one of the two tiles with the shared edge).
 *      Replacing a tile has similar subactions to 'build' (there is no rotation):
 *      select/change replacement tile,
 *      move replacement tile to replacement location by selecting that location.
 * rebuild = rebuild the board (after a shared edge has been changed) by adding a tile to the board.
 *      Rebuilding the board has the same subactions as 'build'.
 * @param {Object} spec
 * @param {gameModelBasics} spec.gameModelBasics
 */


:- module(game_model_tiles,
    [init_game_model_tiles/0, save_game_model_tiles/0, load_game_model_tiles/0,
     save_game_model_tiles_stream/1, retract_game_model_tiles/0,
     get_tiles/1, get_total_tiles_in_game/1, get_board/1, get_hands/1, get_hand/2,
     get_turn/1, set_turn/1, increment_turn/1, get_turn_after_resolution/1, set_turn_after_resolution/1,
     get_selected_tile_id/1, set_selected_tile_id/1, get_selection_marker/1, update_selection_marker/0,
     get_replacements/1, set_replacements/1, remove_tile_from_replacements/1,
     get_mismatches/1, set_mismatches/1,
     get_board_tile_by_grid/2, get_last_build_phase_tile_placed/1,
     set_last_build_phase_tile_placed/1,
     last_placed_tiles/2, place_tile_in_hand/1, place_tiles_in_hand/1, place_tile_on_board/3,
     get_game_phase/1, update_game_phase/0, update_game_phase/2, get_game_phase_status/1, set_game_phase_status/1,
     edge_neighbor_tile/3, edge_to_neighbor_edge/2, tile_in_inactive_hand/1,
     tile_in_active_hand/1, tile_in_board/1, get_tiles_placed/1, game_model_tiles_values/1,
     undo_phase_updates/0, undo_selection_updates/1, write_undo_history/1]).

:- use_module('../proscriptls_sdk/library/data_predicates').
:- use_module(model_basics).
:- use_module(tile_model).
:- use_module(view_basics).
:- use_module(library).

:- initialization(initdyn).

initdyn :-
    data_predicate_dynamics([
        data_predicates(gmt, data, [undoable],
            [tile_counter, tiles, hands, board,
             tilesPlaced, boardHash, mismatches,
             lastPlacedTile1, lastPlacedTile2, lastBuildPhaseTilePlacedID,
             turn, turnAfterResolution, selectedTileID, replacements, gamePhase, gamePhaseStatus, selectionMarker
            ])]).

dummy_reference :-
    dummy_reference,
    abbrev(_,_,_).

write_undo_history(Marker) :-
    findall(A, (undoable_term(X, Y), abbrev(Y, X, A)), Xs),
    data_default_id(ID),
    abbrev1(game_model_tiles:data_selectionMarker(ID, Marker), MA),
    write_undo_history(Xs, MA).

write_undo_history([H|T], MA) :-
    writeln(H),
    (H=MA -> true
    ;
     write_undo_history(T, MA)
    ).

abbrev(_ : '$none', X, - Abbrev) :-
    !,
    abbrev1(X, Abbrev).
abbrev(Y, _ : '$none', + Abbrev) :-
    !,
    abbrev1(Y, Abbrev).
abbrev('$none', X, - Abbrev) :-
    !,
    abbrev1(X, Abbrev).
abbrev(Y, '$none', + Abbrev) :-
    !,
    abbrev1(Y, Abbrev).
abbrev(Y, _X, Abbrev) :-
    abbrev1(Y, Abbrev).

abbrev1(Module : BaseGoal, Abbrev) :-
    Module : data_predicates(_, Prefix, _, _),
    atom_codes(Prefix, PrefixCodes),
    append(PrefixCodes, "_", CombinedPrefixCodes),
    functor(BaseGoal, F, _),
    atom_codes(F, FCodes),
    append(CombinedPrefixCodes, NameCodes, FCodes),
    condense_name(NameCodes, CondensedNameCodes),
    atom_codes(Name, CondensedNameCodes),
    ((Module = tile_model; Module = tile_view; Module = location_model)
        -> arg(1, BaseGoal, V1),
           arg(2, BaseGoal, V2),
           Abbrev =.. [Name, V1, V2]
    ;
    arg(2, BaseGoal, Value),
    Abbrev =.. [Name, Value]
    ).

% remove vowels from NameCodes after first letter.
condense_name([H|T], [H|CondensedTail]) :-
    condense_name1(T, CondensedTail).

condense_name1([], []).
condense_name1([H|T], CondensedNameCodes) :-
    condense_name2(H, CondensedNameCodes, CondensedTail),
    condense_name1(T, CondensedTail).

condense_name2(H, CondensedNameCodes, CondensedTail) :-
    member(H, "aeiouy")
      -> CondensedNameCodes = CondensedTail
    ;
    CondensedNameCodes = [H|CondensedTail].

init_game_model_tiles :-
    assert_data(gmt(0, [], [], [], 0, [], [], none, none, none, 0, none, none, [], none, closed, 0), 1),
    build_tiles.

save_game_model_tiles_stream(Stream) :-
    save_data_stream(data, Stream).

retract_game_model_tiles :-
    retract_all_data(data).

save_game_model_tiles :-
    save_data(data, local_storage('mosaic')).

load_game_model_tiles :-
    load_data(data, local_storage('mosaic')).

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
    data_tile_counter(GameModelTilesID, OldCounter),
    NewCounter is OldCounter + 1,
    update_data_tile_counter(GameModelTilesID, OldCounter, NewCounter).

set_tile_counter(Counter) :-
    data_default_id(GameModelTilesID),
    set_data_tile_counter(GameModelTilesID, Counter).

set_hands(HandTiles) :-
    data_default_id(GameModelTilesID),
    set_data_hands(GameModelTilesID, HandTiles).

set_tiles(Tiles) :-
    data_default_id(GameModelTilesID),
    set_data_tiles(GameModelTilesID, Tiles).

get_tiles(T) :- data_tiles(T).

get_total_tiles_in_game(Count) :- data_tile_counter(Count).

get_board(Board) :- data_board(Board).

get_hands(Hands) :- data_hands(Hands).

get_hand(PlayerID, Hand) :-
    data_hands(Hands),
    nth1(PlayerID, Hands, Hand).

add_hand_tile(PlayerID, Tile) :-
    data_default_id(ID),
    data_hands(ID, Hands),
    add_hand_tile(Hands, PlayerID, Tile, NewHands),
    update_data_hands(ID, Hands, NewHands).

add_hand_tile([H|T], 1, Tile, [[Tile|H]|T]) :-
    !.
add_hand_tile([H|T], IDCounter, Tile, [H|TN]) :-
    NextIDCounter is IDCounter - 1,
    add_hand_tile(T, NextIDCounter, Tile, TN).

get_turn(Turn) :- data_turn(Turn).

set_turn(Turn) :-
    data_default_id(GameModelTilesID),
    set_data_turn(GameModelTilesID, Turn).

increment_turn(NewTurn) :-
    data_default_id(GameModelTilesID),
    data_turn(GameModelTilesID, OldTurn),
    get_number_of_players(NumberOfPlayers),
    NewTurn is (OldTurn mod NumberOfPlayers) + 1,
    update_data_turn(GameModelTilesID, OldTurn, NewTurn).

get_turn_after_resolution(Turn) :- data_turnAfterResolution(Turn).

set_turn_after_resolution(Turn) :-
    data_default_id(GameModelTilesID),
    set_data_turnAfterResolution(GameModelTilesID, Turn).

get_selected_tile_id(Selected) :-
    data_selectedTileID(Selected).

set_selected_tile_id(Selected) :-
    data_default_id(GameModelTilesID),
    set_data_selectedTileID(GameModelTilesID, Selected).

get_selection_marker(Marker) :-
    data_selectionMarker(Marker).

update_selection_marker :-
    data_selectionMarker(GameModelTilesID, Old),
    New is Old + 1,
    data_default_id(GameModelTilesID),
    update_data_selectionMarker(GameModelTilesID, Old, New).

get_replacements(Replacements) :-
    data_replacements(Replacements).

set_replacements(Replacements) :-
    data_default_id(GameModelTilesID),
    set_data_replacements(GameModelTilesID, Replacements).

remove_tile_from_replacements(Tile) :-
    data_default_id(GameModelTilesID),
    data_replacements(GameModelTilesID, Replacements),
    delete(Replacements, Tile, NewReplacements),
    update_data_replacements(GameModelTilesID, Replacements, NewReplacements).

get_board_tile_by_grid(GridX > GridY, TileID) :-
    board_hash_key_coords(GridX, GridY, Key),
    data_boardHash(BoardHash),
    member(Key-TileID, BoardHash).

get_mismatches(Value) :-
    data_mismatches(Value).

set_mismatches(Value) :-
    data_default_id(GameModelTilesID),
    set_data_mismatches(GameModelTilesID, Value).

add_board_mismatches(NewMismatches) :-
    data_default_id(GameModelTilesID),
    data_mismatches(GameModelTilesID, OldMismatches),
    append(NewMismatches, OldMismatches, RawMismatches),
    sort(RawMismatches, Mismatches),
    update_data_mismatches(GameModelTilesID, OldMismatches, Mismatches).

get_last_build_phase_tile_placed(ID) :-
    data_lastBuildPhaseTilePlacedID(ID).

set_last_build_phase_tile_placed(ID) :-
    data_default_id(GameModelTilesID),
    set_data_lastBuildPhaseTilePlacedID(GameModelTilesID, ID).

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

add_board_tile(Tile) :-
    data_default_id(ID),
    update_data_board(ID, Board, [Tile|Board]).

remove_tile_from_board(Tile) :-
    data_default_id(GameModelTilesID),
    data_board(GameModelTilesID, Board),
    delete(Board, Tile, RemainderBoard),
    update_data_board(GameModelTilesID, Board, RemainderBoard).

remove_tile_from_board_hash(Tile) :-
    tile_board_hash_key(Tile, Key),
    data_default_id(GameModelTilesID),
    data_boardHash(GameModelTilesID, BoardHash),
    delete(BoardHash, Key-Tile, RemainderBoardHash),
    update_data_boardHash(GameModelTilesID, BoardHash, RemainderBoardHash).

add_board_hash_tile(Tile) :-
    tile_board_hash_key(Tile, Key),
    data_default_id(GameModelTilesID),
    update_data_boardHash(GameModelTilesID, BoardHash, [Key-Tile|BoardHash]).

remove_tile_from_container(Tile) :-
    get_tile_container(Tile, Container),
    (Container = hand(PlayerID)
      -> data_default_id(GameModelTilesID),
         data_hands(GameModelTilesID, Hands),
         remove_tile_from_hand(Hands, Tile, PlayerID, NewHands),
         update_data_hands(GameModelTilesID, Hands, NewHands)
    ;
     Container = board
      -> data_default_id(GameModelTilesID),
         remove_tile_from_board_hash(Tile),
         remove_tile_from_board(Tile)
    ;
     throw(mosaic_internal('invalid container type', Container))
    ).

remove_tile_from_hand([Hand|T], Tile, 1, [NewHand|T]) :-
    !,
    delete(Hand, Tile, NewHand).
remove_tile_from_hand([H|T], Tile, PlayerCounter, [H|NewHandsTail]) :-
    PlayerCounter > 1,
    NextPlayerCounter is PlayerCounter - 1,
    remove_tile_from_hand(T, Tile, NextPlayerCounter, NewHandsTail).

place_tiles_in_hand([]).
place_tiles_in_hand([H|T]) :-
    place_tile_in_hand(H),
    place_tiles_in_hand(T).

place_tile_in_hand(Tile) :-
    remove_tile_from_container(Tile),
    update_grid_x(Tile, _, 0),
    update_grid_y(Tile, _, 0),
    data_turn(Turn),
    update_container(Tile, _, hand(Turn)),
    add_hand_tile(Turn, Tile).

place_tile_on_board(Tile, GridX, GridY) :-
    get_tile_container(Tile, Container),
    (Container = hand(_PlayerID)
      -> update_grid_x(Tile, _, GridX),
         update_grid_y(Tile, _, GridY),
         add_board_tile(Tile),
         add_board_hash_tile(Tile),
         remove_tile_from_container(Tile),
         update_container(Tile, _, board),
         get_game_phase(Phase),
         (Phase = build
           -> set_last_build_phase_tile_placed(Tile)
          ;
          set_last_build_phase_tile_placed(none),
          neighbor_mismatches(Tile, Mismatches),
          add_board_mismatches(Mismatches)
         )
    ;
     Container = board
      -> remove_tile_from_board_hash(Tile),
         update_grid_x(Tile, _, GridX),
         update_grid_y(Tile, _, GridY),
         add_board_hash_tile(Tile)
    ;
     throw(mosaic_internal('invalid container type (place_tile_on_board)', Container))
    ).

neighbor_mismatches(Tile, Mismatches) :-
    get_tile_colors(Tile, Colors),
    get_triangles_per_tile(TPT),
    neighbor_mismatches(TPT, Tile, Colors, Mismatches).

% neighbor_mismatches(NumberOfEdgesToInspect, Tile, Colors, Mismatches).
% Edge is a number from 0 to (TrianglesPerTile-1)
neighbor_mismatches(NumberOfEdgesToInspect, Tile, Colors, Mismatches) :-
    Edge is NumberOfEdgesToInspect - 1,
    neighbor_mismatch(Edge, Tile, Colors, Mismatches, Tail),
    (Edge > 0
      -> neighbor_mismatches(Edge, Tile, Colors, Tail)
    ;
    Tail = []
    ).


neighbor_mismatch(Edge, Tile, TileColors, Mismatches, Tail) :-
    edge_neighbor_tile(Tile, Edge, NeighborID)
      -> get_tile_colors(NeighborID, NeighborColors),
         nth0(Edge, TileColors, TileColor),
         edge_to_neighbor_edge(Edge, NeighborEdge),
         (nth0(NeighborEdge, NeighborColors, TileColor)
           -> Mismatches = Tail
         ;
         Mismatches = [NeighborID|Tail]
         )
    ;
    Mismatches = Tail.


get_game_phase(Phase) :-
    data_gamePhase(Phase).

set_game_phase(Phase) :-
    data_default_id(GameModelTilesID),
    set_data_gamePhase(GameModelTilesID, Phase).

update_game_phase :-
    update_game_phase(_, _).

update_game_phase(Old, New) :-
    data_gamePhase(Old),
    get_game_phase_status(Status),
    (Status = open
        -> Old = New
    ;
    update_closed_game_phase(Old, New),
    set_game_phase_status(open)
    ).

update_closed_game_phase(Old, New) :-
    Old = none
      -> New = build,
         set_game_phase(New)
    ;
    (Old = build; Old = rebuild),
    get_replacements([]),
    data_board(Board),
    length(Board, BoardLength),
    data_tiles(Tiles),
    length(Tiles, TilesLength),
    BoardLength = TilesLength
      -> New = transform,
         set_game_phase(New)
    ;
    Old = transform,
    get_replacements(Replacements),
    Replacements \= []
      -> New = replace,
         set_game_phase(New)
    ;
    Old = rebuild,
    get_replacements(Replacements),
    Replacements \= []
      -> New = replace,
         set_game_phase(New)
    ;
     Old = replace,
     get_replacements(Replacements),
     \+ (member(Replacement, Replacements),
         get_tile_container(Replacement, board)),
     get_hands(Hands),
     (Hands \= [[],[]]
      -> New = rebuild,
         set_game_phase(New)
     ;
      New = transform,
      set_game_phase(New)
     )
    ;
    Old = New.

get_game_phase_status(Value) :-
    data_gamePhaseStatus(Value).

set_game_phase_status(Value) :-
    data_default_id(GameModelTilesID),
    set_data_gamePhaseStatus(GameModelTilesID, Value).

edge_to_neighbor_edge(Edge, NeighborEdge) :-
    get_triangles_per_tile(TPT), % TPT is 4 or 6. A TPT of 3 would require a different analysis.
    NeighborEdge is ((Edge + (TPT/2)) mod TPT).

edge_neighbor_tile(ID, Edge, NeighborID) :-
    edge_neighbor_position(ID, Edge, Position),
    get_board_tile_by_grid(Position, NeighborID).

container_id(hand(ID), ID).
container_type(hand(_ID), hand).
container_type(board, board).

tile_in_inactive_hand(Tile) :-
    get_tile_container(Tile, Container),
    container_type(Container, hand),
    get_turn(TurnID),
    \+ container_id(Container, TurnID).

tile_in_active_hand(Tile) :-
    get_tile_container(Tile, Container),
    container_type(Container, hand),
    get_turn(TurnID),
    container_id(Container, TurnID).

tile_in_board(Tile) :-
    get_tile_container(Tile, Container),
    container_type(Container, board).

get_tiles_placed(Placed) :-
    data_tilesPlaced(Placed).

game_model_tiles_values(Values) :-
    data_default_id(ID),
    labelled_values(data, ID, Values).

% undo all undoable updates from current closed game phase back to most recent previous closed game phase.
undo_phase_updates :-
    data_default_id(ID),
    data_gamePhaseStatus(ID, closed), % the undo can only be run when the data is currently 'closed'.
    undo_update(data_gamePhaseStatus(ID, open), data_gamePhaseStatus(ID, closed)), % first back up to data with most recent 'open' state.
    undo_update(data_gamePhaseStatus(ID, closed), _). % undo back through many data updates to when data was next most recently 'closed'.

% undo all undoable updates back to specified selection marker value.
undo_selection_updates(Marker) :-
    data_default_id(ID),
    undo_update(_, data_selectionMarker(ID, Marker)).
