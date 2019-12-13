:- module(tiles, [setup_game_data/0, start_mosaic_game/0, clear_mosaic_game/0, score_delay1/1,
        save_game_stream/0, load_game/0, display_game/0, on_click_tile_rotate/3, reposition_board_loop/0]).

:- use_module('../proscriptls_sdk/library/object'). % for >>/2.
%:- use_module('../proscriptls_sdk/library/data_predicates').
:- use_module(library).
:- use_module(model_basics).
:- use_module(view_basics).
:- use_module(tile_model).
:- use_module(game_model_tiles).
:- use_module(location_model).
:- use_module(locations).
:- use_module(tile_view).
:- use_module(game_view_tiles).
:- use_module(draw).
:- use_module(score).

dummy_reference :-
    dummy_reference,
    select(_),
    load_game_data,
    save_game,
    calculate_and_display_score.

setup_game_data :-
    init_model_basics(2, 4, [1,2,3,4]),
    init_game_model_tiles, % uses info in model_basics.
    update_game_phase,
    increment_turn(1), % the first increment should move the turn from the initial 0 to 1.
    create_view_basics,
    create_game_view_tiles,
    layout_hands,
    create_locations,
    find_shaped_locations,
    create_score,
    !.

start_mosaic_game :-
    setup_game_data,
    get_canvas_width(W),
    get_canvas_height(H),
    get_context(Ctx),
    _Canvas >> [id -:> canvas,
        addEventListener(click, [object-E]^select(E))],
    get_tiles(TileIDs),
    draw_all_tiles(TileIDs, Ctx, W, H).

clear_mosaic_game :-
    get_canvas_width(W),
    get_canvas_height(H),
    get_context(Ctx),
    _Canvas >> [id -:> canvas,
        removeEventListener(click, [object-E]^select(E))],
    Ctx >*> clearRect(0, 0, W, H),
    set_selected_tile_id(none).

save_game_stream :-
    writeln('Saving...'),
    new_memory_file(DataMemFile),
    open_memory_file(DataMemFile, write, Stream),
    writeln('  model_basics'),
    save_model_basics_stream(Stream),
    writeln('  view_basics'),
    save_view_basics_stream(Stream),
    writeln('  tile_model'),
    save_tile_model_stream(Stream),
    writeln('  tile_view'),
    save_tile_view_stream(Stream),
    writeln('  game_model_tiles'),
    save_game_model_tiles_stream(Stream),
    writeln('  game_view_tiles'),
    save_game_view_tiles_stream(Stream),
    writeln('  location_model'),
    save_location_model_stream(Stream),
    writeln('  locations'),
    save_locations_stream(Stream),
    writeln('  score'),
    save_score_stream(Stream),
    writeln('  (finish)'),
    close(Stream),
    copy_memory_file_to_local_storage(DataMemFile, save_mosaic),
    free_memory_file(DataMemFile),
    writeln('Done.').

load_game :-
    writeln('Load Game.'),
    load_game_data_stream,
    setup_event_handling,
    display_game,
    yield,
    setup_score.

setup_score :-
    get_components(Components),
    components_score(Components, Scores),
    display_score(Scores).
%
%    clear_score,
%    create_score,
%    calculate_and_display_score.

% setup_event_handling prepares the game to accept mouse clicks.
% removeEventListener in case one is already present - this method succeeds even if there is no
% handler present.

setup_event_handling :-
    _Canvas >> [id -:> canvas,
        removeEventListener(click, [object-E]^select(E))],
    _Canvas >> [id -:> canvas,
        addEventListener(click, [object-E]^select(E))].

display_game :-
    get_canvas_width(W),
    get_canvas_height(H),
    get_context(Ctx),
    get_tiles(TileIDs),
    draw_all_tiles(TileIDs, Ctx, W, H).

load_game_data_stream :-
    retract_game_data,
    copy_local_storage_to_memory_file(save_mosaic, DataMemFile),
    wam_compiler:compile_and_free_memory_file(DataMemFile),
    reset_view_basics. % reset the HTML UI values for canvas HTML element.

retract_game_data :-
    retract_model_basics,
    retract_view_basics,
    retract_tile_model,
    retract_tile_view,
    retract_game_model_tiles,
    retract_game_view_tiles,
    retract_location_model,
    retract_locations.

save_game :-
    writeln('Saving...'),
    writeln('  model_basics'),
    save_model_basics,
    writeln('  view_basics'),
    save_view_basics,
    writeln('  tile_model'),
    save_tile_model,
    writeln('  tile_view'),
    yield,
    save_tile_view,
    writeln('  game_model_tiles'),
    save_game_model_tiles,
    writeln('  game_view_tiles'),
    save_game_view_tiles,
    writeln('  location_model'),
    save_location_model,
    writeln('  locations'),
    save_locations,
    writeln('Done.').

load_game_data :-
    load_game_data1,
    load_game_data1a,
    load_game_data2,
    load_game_data3.

load_game_data1 :-
    writeln('Loading...'),
    writeln('  model_basics'),
    yield,
    load_model_basics,
    writeln('  tile_model'),
    yield,
    load_tile_model.

load_game_data1a :-
    writeln('  view_basics'),
    yield,
    load_view_basics.

load_game_data2 :-
    writeln('  game_model_tiles'),
    yield,
    load_game_model_tiles,
    writeln('  game_view_tiles'),
    yield,
    load_game_view_tiles.

load_game_data3 :-
    writeln('  location_model'),
    yield,
    load_location_model,
    writeln('  locations'),
    yield,
    load_locations,
    writeln('  tile_view'),
    yield,
    load_tile_view,
    writeln('Done.'),
    yield.

 % clientX and clientY are coordinates within the containing HTMLCanvasElement
 % It appears that the rendering coordinates (e.g. moveTo(RX, RY)) are coordinates
 % within the containing HTMLCanvasElement minus the canvas offset.
 % RX = clientX - offsetX.

select(Event) :-
    Event >> [pageX +:> PageX, pageY +:> PageY],
    dom_release_object(Event),
    select1(PageX, PageY).

select1(PageX, PageY) :-
    setup_select1(PageX, PageY, X, Y),
    process_select(X, Y),
    update_game_phase.

setup_select1(PageX, PageY, X, Y) :-
    get_canvas_offset_top(PTop),
    get_canvas_offset_left(PLeft),
    X is PageX - PLeft,
    Y is PageY - PTop,
    writeln(select(PageX, PageY, PLeft, PTop, X, Y)).

process_select(X, Y) :-
    point_in_tile(Tile, X, Y)
      -> on_click_tile(Tile, X, Y)  % at most one tile contains (X, Y).
    ;
     point_in_legal_location(BX, BY, X, Y)
      -> on_click_legal_location(BX, BY)
    ;
     true.

on_click_tile(ID, X, Y) :-
    %writeln(click(ID, X, Y)),
    get_game_phase(Phase),
    (tile_in_active_hand(ID),
     (Phase = build;Phase = rebuild)
      -> on_click_active_hand_tile(ID)
    ;
    tile_in_board(ID)
      -> (Phase = transform
          -> on_click_transform_tile(ID, X, Y)
         ;
         Phase = replace
          -> on_click_select_replace_tile(ID)
         ;
         true
         )
    ;
    true
    ).

on_click_active_hand_tile(ID) :-
    _ >> [id -:> canvas, getContext('2d') *:> Ctx],
    (get_selected_tile_id(OldID)
    ;
     OldID = none
    ),
    (OldID = ID
      -> on_click_selected_tile_rotate(ID)
    ;
    OldID \= none
      -> set_selected_tile_id(none),
         draw_all_tile(OldID, Ctx), % de-select OldID
         on_click_active_hand_tile_select(ID)
    ;
    on_click_active_hand_tile_select(ID)
    ).

on_click_active_hand_tile_select(ID) :-
    hand_tile_selectable(ID)
      -> set_selected_tile_id(ID),
         _ >> [id -:> canvas, getContext('2d') *:> Ctx],
         draw_all_tile(ID, Ctx),
         find_legal_with_rotation_locations(ID),
         find_legal_locations(ID),
         get_legal_positions(LegalPositions),
         get_legal_positions_with_rotation(LegalPositionsWithRotation),
         draw_legal_moves(LegalPositions, LegalPositionsWithRotation, Ctx)
    ;
    true.

hand_tile_selectable(ID) :-
    get_replacements(Replacements),
    hand_tile_selectable(ID, Replacements).

hand_tile_selectable(_ID, []) :-
    !.
hand_tile_selectable(ID, Replacements) :-
    member(ID, Replacements),
    !.
hand_tile_selectable(ID, Replacements) :-
    member(ID - _, Replacements),
    !.

on_click_select_replace_tile(ID) :-
    (get_selected_tile_id(OldID)
    ;
     OldID = none
    ),
    (OldID = ID
      -> on_click_selected_tile_rotate(ID)
    ;
    _ >> [id -:> canvas, getContext('2d') *:> Ctx],
    (OldID \= none
      -> set_selected_tile_id(none),
         draw_all_tile(OldID, Ctx) % de-select OldID
     ;
     true
    ),
    set_selected_tile_id(ID),
    draw_all_tile(ID, Ctx),
    find_legal_with_rotation_locations(ID),
    find_legal_locations(ID),
    get_legal_positions(LegalPositions),
    get_legal_positions_with_rotation(LegalPositionsWithRotation),
    draw_legal_moves(LegalPositions, LegalPositionsWithRotation, Ctx)
    ).

on_click_legal_location(BX, BY) :-
    get_selected_tile_id(Tile),
    set_selected_tile_id(none),
    _ >> [id -:> canvas, getContext('2d') *:> Ctx],
    get_game_phase(Phase),
    (Phase = build
      -> set_game_phase_status(closed),
         increment_turn(_),
         place_tile_on_board_and_draw(Ctx, Tile, BX, BY),
         score_delay(Tile)
    ;
     Phase = rebuild
       -> get_replacements(OldReplacements),
          place_tile_on_board_and_draw(Ctx, Tile, BX, BY),
          yield,
          get_replacements(Replacements),
          clear_discarded_replacement_views(Ctx, OldReplacements, Replacements),
          (get_turn(Turn),
           get_hand(Turn, Hand),
           Hand = []
           % four cases based on two issues: mismatches-to-resolve and in-resolution-sequence
            -> set_game_phase_status(closed),
               get_turn_after_resolution(AfterResolutionTurn),
               (get_mismatches(Mismatches),
                Mismatches = []
                 -> (AfterResolutionTurn = none
                      -> increment_turn(_)
                    ;
                     set_turn(AfterResolutionTurn),
                     set_turn_after_resolution(none)
                    )
               ;
                increment_turn(NextTurn),
                process_mismatches, % This continues the resolution sequence. The phase will be updated to 'replace' by update_game_phase.
                (AfterResolutionTurn = none
                  -> set_turn_after_resolution(NextTurn)
                ;
                true
                )
               )
          ;
          true
          )
    ;
     Phase = replace
      -> get_replacements(OldReplacements),
         move_tile_on_board_and_draw(Ctx, Tile, BX, BY),
         yield,
         %update_replacements,
         get_replacements(Replacements),
         clear_discarded_replacement_views(Ctx, OldReplacements, Replacements),
         (member(Replacement, Replacements),
          get_tile_container(Replacement, board) % replacements can be in the hand (for rebuild phase) if there are mismatch resolutions required.
           -> true
         ;
         find_rebuild_hole_shaped_locations
           -> set_game_phase_status(closed),
              get_turn(Turn),
              get_hand(Turn, HandTiles),
              get_shaped_positions(HoleShapedLocations),
              %update_game_phase, % force the game phase to be 'rebuild' - the game state after find_replacements/2 looks the same as for replace.
              find_replacements(HandTiles, HoleShapedLocations),
              get_replacements(NewReplacements),
              draw_constrained_replacements(NewReplacements, Ctx)
         ;
         true
           -> set_game_phase_status(closed),
              find_shaped_locations % board has been changed from replacements, now set up shaped locations to rebuild from hand.
         )
    ;
    writeln(legal_location_click_error(Tile, BX, BY, Phase))
    ).

% process_mismatches sets up another hand, shaped locations,
% and replacement tiles - continuing the resolution sequence.
% The phase will be updated to 'replace' by update_game_phase.

process_mismatches :-
    get_mismatches(Mismatches),
    set_mismatches([]),
    create_mismatch_shaped_locations(Mismatches),
    setup_replacements(Mismatches),
    draw_game_tiles.

draw_constrained_replacements([], _Ctx).
draw_constrained_replacements([H|T], Ctx) :-
    draw_replacement_tile_mark(H, Ctx),
    draw_constrained_replacements(T, Ctx).

clear_discarded_replacement_views(Ctx, OldReplacements, Replacements) :-
    forall((member(Tile, OldReplacements), \+ member(Tile, Replacements)),
        draw_all_tile(Tile, Ctx)).

on_click_tile_rotate(ID, _X, _Y) :-
    %writeln(click(ID, X, Y)),
    (tile_in_active_hand(ID)
      -> on_click_selected_tile_rotate(ID)
    ;
    true % writeln(not_active)
    ).

on_click_selected_tile_rotate(ID) :-
    _ >> [id -:> canvas, getContext('2d') *:> Ctx],
    tile_rotate_right(ID),
    draw_all_tile(ID, Ctx),
    get_shaped_positions(OldLocations),
    clear_location_views(OldLocations, Ctx),
    find_legal_locations(ID),
    get_legal_positions(LegalPositions),
    get_legal_positions_with_rotation(LegalPositionsWithRotation),
    draw_legal_moves(LegalPositions, LegalPositionsWithRotation, Ctx).

point_in_legal_location(BX, BY, X, Y) :-
     wam_duration(Start),
     get_legal_positions(LegalPositions),
     member(LegalPosition, LegalPositions),
     get_location_grid_x(LegalPosition, BX),
     get_location_grid_y(LegalPosition, BY),
     point_in_board_position(BX, BY, X, Y),
     wam_duration(End),
     !,
     display_spans([Start, End], point_in_board).

% transform:
% - identify selected edge,
% - from edge determine selected pair of tiles,
% - determine candidate replacement tiles - set replacements,
% - move selected tiles to current turn player's hand,
% - specify original positions of selected tiles as
% shaped locations (thus 'find legal with rotations'
% and 'find legal' will pick among these original positions).
%
%    Mark removed tile locations as 'replacement positions' with color constraints.
%    Do this mark before moving the marked tiles to hands so that the boardTileX and
%    boardTileY values are 'in the board'.
%
%    The replacement positions are marked by recording them in the legalPositions
%    global array, same as is used for guiding the placement of tiles from the hands
%    during the 'build' state of the game.


on_click_transform_tile(Tile, X, Y) :-
    %_ >> [id -:> canvas, getContext('2d') *:> Ctx],
    writeln(transform(Tile, X, Y)),
    point_in_tile_edge(Tile, X, Y, Edge),
    get_tile_colors(Tile, Colors),
    nth0(Edge, Colors, Color),
    get_turn(PlayerColor),
    (PlayerColor = Color
      -> true % The edge is the current player's color (and thus cannot be *changed* to the current player's color). Ignore this selection click.
    ;
     writeln(transform_edge(Edge, Color)),
     edge_neighbor_tile(Tile, Edge, NeighborTile)
      -> (last_placed_tiles(Tile, NeighborTile)
           -> true % these tiles were placed by previous turn/player. Ignore this selection click.
         ;
          set_game_phase_status(closed),
          create_transform_shaped_locations(Tile, Edge, NeighborTile),
          setup_replacements([Tile, NeighborTile]),
          draw_game_tiles
         )
    ;
     true % There is no neighboring tile for this edge. Ignore this selection click.
    ).

setup_replacements(TilesForHand) :-
    get_shaped_positions(ShapedLocations),
    get_board(BoardTiles),
    find_replacements(BoardTiles, ShapedLocations),
    place_tiles_in_hand(TilesForHand),
    layout_hands.

draw_game_tiles :-
    get_context(Ctx),
    draw_game_tiles(Ctx).

draw_game_tiles(Ctx) :-
    get_canvas_width(W),
    get_canvas_height(H),
    get_tiles(TileIDs),
    draw_all_tiles(TileIDs, Ctx, W, H).

% Tile is replacing a tile that was moved out of the
% board to a hand (due to a transform selection or
% to a mismatch).
% Tile is in Replacements.

move_tile_on_board_and_draw(Ctx, Tile, X, Y) :-
    writeln(move_tile_on_board_and_draw(Tile, X, Y)),
    get_tile_grid_x(Tile, OldX),
    get_tile_grid_y(Tile, OldY),
    add_rebuild_position(OldX > OldY), % check if this position is a hole when ready to start rebuilding.
    place_tile_on_board(Tile, X, Y),

    get_shaped_positions(OldLocations),
    clear_location_views(OldLocations, Ctx),
    set_legal_positions_with_rotation([]),
    set_legal_positions([]),

    remove_tile_from_replacements(Tile),
    clear_shaped_location_for_tile(Tile),
    update_board_tile_view(Tile),
    draw_game_tiles(Ctx).

place_tile_on_board_and_draw(Ctx, Tile, X, Y) :-
    wam_duration(Start),
    place_tile_on_board(Tile, X, Y),

    wam_duration(Mark1),
    get_shaped_positions(OldLocations),
    clear_location_views(OldLocations, Ctx),
    set_legal_positions_with_rotation([]),
    set_legal_positions([]),

    remove_tile_from_replacements(Tile),
    clear_shaped_location_for_tile(Tile),

    wam_duration(Mark2),
    update_board_tile_view(Tile),

    wam_duration(Mark3),
    draw_game_tiles(Ctx),
    reposition_board_loop_delay,

    wam_duration(Mark4),
    incremental_find_shaped_locations(Tile),

    wam_duration(End),
    !,
    display_spans([Start, Mark1,Mark2, Mark3,  Mark4, End], place_tile_on_board_and_draw).

reposition_board_loop_delay :-
    yield,
    reposition_board_loop.
%    eval_javascript("setTimeout(() => proscriptls('tiles:reposition_board_loop'), 30);").

reposition_board_loop :-
    reposition_board_toward_target_translation
      -> _ >> [id -:> canvas,
            getContext('2d') *:> Ctx,
            width +:> W,
            height +:> H],
        get_tiles(TileIDs),
        draw_all_tiles(TileIDs, Ctx, W, H),
        get_legal_positions(LegalPositions),
        get_legal_positions_with_rotation(LegalPositionsWithRotation),
        draw_legal_moves(LegalPositions, LegalPositionsWithRotation, Ctx),
        reposition_board_loop_delay
    ;
    true. % calculate_and_display_score.

score_delay(Tile) :-
    writeln(score_delay(Tile)),
    yield,
    number_codes(Tile, TileCodes),
    append_lists(["setTimeout(() => proscriptls('tiles:score_delay1(", TileCodes, ")'), 0);"], MsgCodes),
    eval_javascript(MsgCodes).

score_delay1(Tile) :-
    writeln(score_delay1(Tile)),
    yield,
    (get_game_phase(build)
      -> incremental_score(Tile, Score)
    ;
     get_game_phase(rebuild),
     get_turn(Turn),
     get_hand(Turn, [])
      -> score(Score)
    ;
     get_game_phase(transform)
      -> score(Score)
    ;
     Score = 'score not available'
    ),
    display_score(Score).

calculate_and_display_score :-
    score(S),
    display_score(S).

display_score(S) :-
    format(atom(Score), '~w', [S]),
    atom_codes(Score, ScoreCodes),
    _ >> [id -:> score, innerHTML <:+ ScoreCodes].