:- module(tiles, [display_title/0, setup_game_data/0, start_mosaic_game/0, clear_mosaic_game/0, score_delay1/1,
        save_game_stream/0, load_game/0, display_game/0, on_click_tile_rotate/3,
        undo_last_selection/0, agent_select/1, apply_clicks/1]).

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
:- use_module(letters).
:- use_module(status).
:- use_module(agent).

:- dynamic(interaction_counter/1).

dummy_reference :-
    dummy_reference,
    select(_),
    load_game_data,
    save_game,
    calculate_and_display_score,
    available_click(_).

display_title :-
    _ >> [id -:> canvas, getContext('2d') *:> Ctx],
    letters:display_letters([m,o,s,a,i,c], Ctx, 20, 100, 100, _).

setup_game_data :-
    init_model_basics(2, 4, [1,2,3,4]),
    init_game_model_tiles, % uses info in model_basics.
    init_agent,
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
    display_status,
    yield,
    setup_score.

setup_score :-
    get_components(Components),
    components_score(Components, Scores),
    display_score(Scores).

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
    reset_view_basics, % reset the HTML UI values for canvas HTML element.
    init_agent.

retract_game_data :-
    retract_model_basics,
    retract_view_basics,
    retract_tile_model,
    retract_tile_view,
    retract_game_model_tiles,
    retract_game_view_tiles,
    retract_location_model,
    retract_locations,
    retract_agent.

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

get_interaction_counter(Count) :-
    interaction_counter(Count)
      -> true
    ;
    Count = 0.

increment_interaction_counter :-
    retract(interaction_counter(Count))
      -> retractall(interaction_counter(_)), % ensure that there are no interaction_counter/1 facts.
         Next is Count + 1,
         asserta(interaction_counter(Next))
    ;
    asserta(interaction_counter(1)).

 % clientX and clientY are coordinates within the containing HTMLCanvasElement
 % It appears that the rendering coordinates (e.g. moveTo(RX, RY)) are coordinates
 % within the containing HTMLCanvasElement minus the canvas offset.
 % RX = clientX - offsetX.

select(Event) :-
    increment_interaction_counter,
    Event >> [pageX +:> PageX, pageY +:> PageY],
    dom_release_object(Event),
    select1(PageX, PageY, display).

select1(PageX, PageY, UI) :-
    setup_select1(PageX, PageY, X, Y),
    process_select(X, Y),
    complete_select(UI).
    %agent_select_delay.

complete_select(UI) :-
    update_game_phase,
    get_game_phase(Phase),
    writeln(game_phase(Phase)),
    yield,
    update_selection_marker,
    (UI = display
      ->  display_status,
          get_game_phase(Phase),
          (Phase \= build % incremental score already called for build.
            -> score_delay
          ;
           true
          )
    ;
    true
    ).

setup_select1(PageX, PageY, X, Y) :-
    get_canvas_offset_top(PTop),
    get_canvas_offset_left(PLeft),
    X is PageX - PLeft,
    Y is PageY - PTop,
    writeln(select(PageX, PageY, PLeft, PTop, X, Y)).

process_select(X, Y) :-
    select_click(X, Y, Click),
    !,
    apply_click(Click).

select_click(X, Y, Click) :-
    available_click(Click),
    position_in_click(Click, X, Y).

/*
select_sequence[597>252, 370>130])
*/

select_sequence([X>Y]) :-
    !,
    select1(X, Y, display).
select_sequence([X>Y, H|T]) :-
    select1(X, Y, no_display),
    !,
    select_sequence([H|T]).

%    writeln(next([H|T])),
%    yield,
%    format(atom(Next), 'setTimeout(() => proscriptls("tiles:select_sequence(~w)"),0);', [T]),
%    atom_codes(Next, NextCodes),
%    eval_javascript(NextCodes).

%
%/*
%Possible actions: select tile (edge) or legal location.
%*/
%process_select(X, Y) :-
%    point_in_tile(Tile, X, Y)
%      -> on_click_tile(Tile, X, Y)  % at most one tile contains (X, Y).
%    ;
%     point_in_legal_location(BX, BY, X, Y)
%      -> on_click_legal_location(BX, BY)
%    ;
%     true.
%
%/*
%Possible select tile actions:
%    - selected a tile in active hand & phase = (build | rebuild) -> select active hand tile
%    - selected a tile on board & phase = transform -> select transformation
%    - selected a tile on board & phase = replace -> select replacement tile
%*/
%on_click_tile(ID, X, Y) :-
%    %writeln(click(ID, X, Y)),
%    get_game_phase(Phase),
%    (tile_in_active_hand(ID),
%     (Phase = build;Phase = rebuild)
%      -> on_click_active_hand_tile(ID)
%    ;
%    tile_in_board(ID)
%      -> (Phase = transform
%          -> on_click_transform_tile(ID, X, Y)
%         ;
%         Phase = replace
%          -> on_click_select_replace_tile(ID)
%         ;
%         true
%         )
%    ;
%    true
%    ).
%
%/*
%Select active hand tile actions:
%    - selected tile already selected -> rotate selected hand tile
%    - selected tile not already selected -> deselected current selected hand tile, if any, and set selected hand tile.
%*/
%on_click_active_hand_tile(ID) :-
%    _ >> [id -:> canvas, getContext('2d') *:> Ctx],
%    current_selected_tile(OldID),
%    (OldID = ID
%      -> on_click_selected_tile_rotate(ID)
%    ;
%    deselect_tile(OldID, Ctx),
%    on_click_active_hand_tile_select(ID)
%    ).

current_selected_tile(ID) :-
    get_selected_tile_id(ID)
    ;
    ID = none.

deselect_tile(ID, Ctx) :-
    ID \= none
      -> set_selected_tile_id(none),
         draw_all_tile(ID, Ctx)
     ;
     true.
/*
Possible set selected hand tile actions:
    - if selected hand tile selectable then record that tile as selected to-be-placed-in-board
      and display its available target locations.
*/
on_click_active_hand_tile_select(ID) :-
    hand_tile_selectable(ID),
      set_selected_tile_id(ID),
         _ >> [id -:> canvas, getContext('2d') *:> Ctx],
         draw_all_tile(ID, Ctx),
         find_legal_with_rotation_locations(ID),
         find_legal_locations(ID),
         draw_locations(Ctx).

hand_tile_selectable(ID) :-
    get_replacements(Replacements),
    hand_tile_selectable(ID, Replacements).

hand_tile_selectable(ID, []) :-
    !,
    find_legal_with_rotation_locations(ID, [_|_]).
hand_tile_selectable(ID, Replacements) :-
    tile_in_replacements(ID, Replacements).

tile_in_replacements(ID) :-
    get_replacements(Replacements),
    tile_in_replacements(ID, Replacements).

tile_in_replacements(ID, Replacements) :-
    member(ID, Replacements),
    !.
tile_in_replacements(ID, Replacements) :-
    member(ID - _, Replacements),
    !.

/*
Possible replacement tile ID actions:
    - if ID is not in replacements then skip.
    - if current selected replacement tile is ID then rotate it
    - otherwise, update selected (replacement) tile to-be-moved/placed-in-board
      to ID and display available target locations
*/
on_click_select_replace_tile(ID) :-
    \+ tile_in_replacements(ID)
      -> true % skip this selection attempt
    ;
    current_selected_tile(OldID),
    (OldID = ID
      -> on_click_selected_tile_rotate(ID)
    ;
    _ >> [id -:> canvas, getContext('2d') *:> Ctx],
    deselect_tile(OldID, Ctx),
    set_selected_tile_id(ID),
    find_legal_with_rotation_locations(ID),
    find_legal_locations(ID),
    draw_all_tile(ID, Ctx),
    draw_locations(Ctx)
    ).

/*
Possible location actions:
    - build phase -> close phase, increment turn, move selected tile to selected location, recalc score

    - rebuild phase, active hand empty, no mismatches, afterResTurn = none -> move selected tile to selected location, close phase, inc turn
    - rebuild phase, active hand empty, no mismatches, afterResTurn \= none -> move selected tile to selected location, close phase, set turn to afterResTurn, set afterResTurn to none
    - rebuild phase, active hand empty, mismatches, afterResTurn = none -> move selected tile to selected location, close phase, inc turn, process_mismatches, set afterResTurn to next turn
    - rebuild phase, active hand empty, mismatches, afterResTurn \= none -> move selected tile to selected location, close phase, inc turn, process_mismatches
    - rebuild phase, no active hand empty -> move selected tile to selected location
    - replace phase, exists replacement on board -> move tile, clear replacements
    - replace phase, no replacement on board, exists hole shape -> move tile, clear replacements, close phase, assign hand tiles as hole replacements
    - replace phase, no replacement on board, no hole shape -> move tile, clear replacements, close phase, find shape locations
    - otherwise, error
*/
on_click_legal_location(BX, BY) :-
    get_selected_tile_id(Tile),
    set_selected_tile_id(none),
    _ >> [id -:> canvas, getContext('2d') *:> Ctx],
    get_game_phase(Phase),
    (Phase = build
      -> set_game_phase_status(closed),
         increment_turn(_),
         place_tile_on_board_and_draw(Ctx, Tile, BX, BY)
%         ,
%         score_delay(Tile)
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
    draw_locations(Ctx).

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

%/*
%Possible transform actions:
%    - PlayerColor \= selected edge Color, selected edge has neighbor, neighboring tiles are not previous edge -> close phase, setup transform locations, setup replacement tiles
%    - otherwise -> skip
%*/
%
%on_click_transform_tile(Tile, X, Y) :-
%    %_ >> [id -:> canvas, getContext('2d') *:> Ctx],
%    writeln(transform(Tile, X, Y)),
%    point_in_tile_edge(Tile, X, Y, Edge),
%    on_click_transform_edge(Tile, Edge).

on_click_transform_edge(Tile, Edge) :-
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

draw_game_tiles_and_locations :-
    get_context(Ctx),
    draw_game_tiles_and_locations(Ctx).

draw_game_tiles_and_locations(Ctx) :-
    get_context(Ctx),
    draw_game_tiles(Ctx),
    draw_locations(Ctx).

draw_game_tiles :-
    get_context(Ctx),
    draw_game_tiles(Ctx).

draw_game_tiles(Ctx) :-
    get_canvas_width(W),
    get_canvas_height(H),
    get_tiles(TileIDs),
    draw_all_tiles(TileIDs, Ctx, W, H).

draw_locations(Ctx) :-
    get_legal_positions(LegalPositions),
    get_legal_positions_with_rotation(LegalPositionsWithRotation),
    draw_legal_moves(LegalPositions, LegalPositionsWithRotation, Ctx).

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
    get_interaction_counter(InteractionCounter),
    reposition_board_loop_delay(InteractionCounter),

    wam_duration(Mark4),
    incremental_find_shaped_locations(Tile),

    wam_duration(End),
    !,
    display_spans([Start, Mark1,Mark2, Mark3,  Mark4, End], place_tile_on_board_and_draw).

reposition_board_loop_delay(InteractionCounter) :-
    yield,
    get_interaction_counter(CurrentCounter),
    (InteractionCounter == CurrentCounter
      -> reposition_board_loop(InteractionCounter)
    ;
    true).
%    eval_javascript("setTimeout(() => proscriptls('tiles:reposition_board_loop'), 30);").

/*
reposition_board_loop(InteractionCounter) :-
    reposition_board_toward_target_translation
      -> _ >>$ [id -:> canvas,
            getContext('2d') *:> Ctx,
            width +:> W,
            height +:> H],
        get_tiles(TileIDs),
        draw_all_tiles(TileIDs, Ctx, W, H),
        draw_locations(Ctx),
        reposition_board_loop_delay(InteractionCounter)
    ;
    true. % calculate_and_display_score.
*/

reposition_board_loop(InteractionCounter) :-
    reposition_board_toward_target_translation,
    !,
    _ >>$ [id -:> canvas,
            getContext('2d') *:> Ctx,
            width +:> W,
            height +:> H],
    get_tiles(TileIDs),
    draw_all_tiles(TileIDs, Ctx, W, H),
    draw_locations(Ctx),
    reposition_board_loop_delay(InteractionCounter).
reposition_board_loop(_InteractionCounter) :-
    true. % calculate_and_display_score.

score_delay :-
    score_delay(0).

score_delay(Tile) :-
%    writeln(score_delay(Tile)),
%    yield,
    number_codes(Tile, TileCodes),
    append_lists(["setTimeout(() => proscriptls('tiles:score_delay1(", TileCodes, ")'), 0);"], MsgCodes),
    eval_javascript(MsgCodes).

score_delay1(Tile) :-
%    writeln(score_delay1(Tile)),
%    yield,
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
     Score = 'score not calculated'
    ),
    display_score(Score).

calculate_and_display_score :-
    score(S),
    display_score(S).

display_score(S) :-
    format(atom(Score), '~w', [S]),
    atom_codes(Score, ScoreCodes),
    _ >> [id -:> score, innerHTML <:+ ScoreCodes],
    !.

undo_last_selection :-
    increment_interaction_counter,
    get_selection_marker(Marker),
    writeln(undo_last_selection(target(Marker))),
    yield,
    undo_selection_updates(Marker),
    writeln(undo_last_selection(finish(basic_undo), start(reconstruct_game_view))),
    yield,
    reconstruct_game_view,
    writeln(undo_last_selection(finish(reconstruct_game_view), start(draw_game_tiles_and_locations))),
    yield,
    draw_game_tiles_and_locations,
    display_status.

reconstruct_game_view :-
    layout_hands,
    reconstruct_board_view.

reconstruct_board_view :-
    get_board(Board),
    reconstruct_board_view(Board),
    get_interaction_counter(Count),
    reposition_board_loop_delay(Count).

reconstruct_board_view([]).
reconstruct_board_view([H|T]) :-
    update_board_tile_view(H),
    reconstruct_board_view(T).

/*
reclick on selected tile
click on unselected tile in hand
click on unselected tile in board
click on (legal) location
click on edge for transform
*/

available_clicks(Clicks) :-
    setof(Click, available_click(Click), Clicks).

available_click(reclick(Tile)) :-
    get_selected_tile_id(Tile),
    Tile \= none.
available_click(click_hand_tile(Tile)) :-
    get_selected_tile_id(none),
    tile_in_active_hand(Tile),
    get_game_phase(Phase),
    (Phase = build;Phase = rebuild).
available_click(click_board_tile(Tile)) :-
    get_selected_tile_id(none),
    tile_in_board(Tile),
    get_game_phase(replace),
    tile_in_replacements(Tile).
available_click(click_edge(Tile, Edge)) :-
    tile_in_board(Tile),
    get_game_phase(transform),
    get_tile_colors(Tile, Colors),
    get_turn(PlayerColor),
    nmember(Color, Colors, EdgePlus),
    Edge is EdgePlus - 1,
    edge_neighbor_tile(Tile, Edge, _NeighborTile),
    %Tile < NeighborTile, % ensure each edge is only selected once.
    PlayerColor \= Color.
available_click(click_location(BX,BY)) :-
    get_legal_positions(Locations),
    member(Location, Locations),
    get_location_grid_x(Location, BX),
    get_location_grid_y(Location, BY).


agent_select_delay :-
    agent_player(Agent),
    get_turn(Agent)
      -> append_lists(["setTimeout(() => proscriptls('tiles:agent_select(_Click)'), 0);"], MsgCodes),
         eval_javascript(MsgCodes)
%         dom_window(_) >*> setTimeout(agent_select(_), 0)
    ;
    true.

agent_select(Click) :-
    get_turn(Agent),
    agent_player(Agent)
      -> ask_agent(Click),
         apply_click(Click),
         complete_select(display),
         agent_select_delay
    ;
    true.

agent_player(2).

ask_agent(Click) :-
    available_clicks(Clicks),
    agent(Clicks, Click).

/*
apply_clicks([click_hand_tile(1), click_location(0,0), click_hand_tile(11), click_location(0,1), click_hand_tile(3), click_location(-1,0), click_hand_tile(9), click_location(1,1), click_hand_tile(8), click_location(-1,1), click_hand_tile(12), reclick(12), click_location(1,2), click_hand_tile(7)]).

apply_clicks([click_edge(16,0), click_board_tile(12), click_location(2,1), click_board_tile(14), reclick(14), click_location(2,0), click_hand_tile(4), reclick(4), reclick(4), reclick(4), click_location(3,2), click_hand_tile(16), reclick(16), click_location(3,-1)]).

tiles:apply_clicks([click_hand_tile(1), click_location(0,0), click_hand_tile(11), click_location(0,1), click_hand_tile(2), click_location(-1,0), click_hand_tile(12), reclick(12), click_location(1,1), click_hand_tile(3)]).

*/

apply_clicks([]).
apply_clicks([H|T]) :-
    apply_click(H),
    apply_clicks1(T).

apply_clicks1([]) :-
    complete_select(display).
apply_clicks1([H|T]) :-
    complete_select(no_display),
    apply_click(H),
    apply_clicks1(T).

apply_click(Click) :-
    get_turn(Turn),
    get_game_phase(Phase),
    writeln(step(Turn, Phase, apply_click(Click))),
    apply_click1(Click),
    !,
    gc.

apply_click1(reclick(Tile)) :-
    on_click_selected_tile_rotate(Tile).
apply_click1(click_hand_tile(Tile)) :-
    on_click_active_hand_tile_select(Tile).
apply_click1(click_board_tile(Tile)) :-
    on_click_select_replace_tile(Tile).
apply_click1(click_edge(Tile, Edge)) :-
    on_click_transform_edge(Tile, Edge).
apply_click1(click_location(BX,BY)) :-
    get_location_grid_x(Location, BX),
    get_location_grid_y(Location, BY),
    on_click_legal_location(BX, BY).

position_in_click(reclick(Tile), X, Y) :-
    point_in_tile(Tile, X, Y).
position_in_click(click_hand_tile(Tile), X, Y) :-
    point_in_tile(Tile, X, Y).
position_in_click(click_board_tile(Tile), X, Y) :-
    point_in_tile(Tile, X, Y).
position_in_click(click_edge(Tile, Edge), X, Y) :-
    point_in_tile_edge(Tile, X, Y, Edge).
position_in_click(click_location(BX,BY), X, Y) :-
    get_location_grid_y(Location, BY),
    get_location_grid_x(Location, BX),
    point_in_legal_location(BX, BY, X, Y).

