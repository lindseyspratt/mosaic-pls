:- module(tiles, [display_title/0, setup_game_data/1, start_mosaic_game/1, clear_mosaic_game/0, score_delay1/1,
        save_game_stream/0, load_game/0, display_game/0, on_click_tile_rotate/3,
        undo_last_selection/0, agent_select/1, apply_clicks/1, steps/1, step_clicks/2, apply_steps/1,
        toggle_auto_play_and_update_button/0, toggle_debugging_and_update_button/0]).

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
:- dynamic(use_auto_play/1).
:- dynamic(step/3).

dummy_reference :-
    dummy_reference,
    select(_),
    load_game_data,
    save_game,
    calculate_and_display_score,
    select_click(_,_,_),
    available_click(_),
    draw_tile_animation1(_,_,_,_,_),
    unbound_action,
    step(_,_,_).

display_title :-
    _ >> [id -:> canvas, getContext('2d') *:> Ctx],
    letters:display_letters([m,o,s,a,i,c], Ctx, 20, 100, 100, _),
    data_mode(Mode),
    setup_undo_button(Mode),
    gc,
    enable_auto_play,
    update_auto_play_buttons,
    disable_debugging,
    update_debugging_button.

setup_undo_button(ephemeral).
setup_undo_button(undoable) :-
    Div >-> id :> undo_button,
    create_dom_element(button, Button),
    Button >> [class <:- 'mosaic-button', addEventListener(click, tiles:undo_last_selection), innerText <:+ "Undo"],
    append_dom_node_child(Div, Button).


setup_game_data(NumberOfPlayers) :-
    init_model_basics(NumberOfPlayers, 4, [1,2,3,4]),
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
    update_auto_play_buttons,
    !.

start_mosaic_game(NumberOfPlayers) :-
    setup_game_data(NumberOfPlayers),
    enable_auto_play,
    get_canvas_width(W),
    get_canvas_height(H),
    get_context(Ctx),
%    _Canvas2 >> [id -:> canvas2, getContext('2d') *:> Ctx2],
    _Canvas >> [id -:> canvas,
        addEventListener(click, [object-E]^select(E))],
%        addEventListener(mousemove, [object-E]^tooltip(E, Ctx2))],
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
    asserta(use_auto_play(true)),
    display_game,
    display_status,
    yield,
    setup_score.

setup_score :-
%    get_components(Components),
%    components_score(Components, _Scores),
    get_totals(Totals),
    display_score(Totals).

% setup_event_handling prepares the game to accept mouse clicks.
% removeEventListener in case one is already present - this method succeeds even if there is no
% handler present.

setup_event_handling :-
    _Canvas >> [id -:> canvas,
        removeEventListener(click, [object-E]^select(E)),
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

enable_auto_play :-
    retractall(use_auto_play(_)),
    asserta(use_auto_play(true)).

disable_auto_play :-
    retractall(use_auto_play(_)),
    asserta(use_auto_play(false)).

toggle_auto_play :-
    use_auto_play(true)
      -> disable_auto_play
    ;
    enable_auto_play.

toggle_auto_play_and_update_button :-
    toggle_auto_play,
    update_auto_play_buttons.

update_auto_play_buttons :-
    use_auto_play(true)
      -> auto_play_message("Disable", AutoMessage, MoveMessage),
         _ >> [id -:> auto_play_button, innerText <:+ AutoMessage],
         _ >> [id -:> move_button, innerText <:+ MoveMessage, disabled <:- true]
    ;
     auto_play_message("Enable", AutoMessage, MoveMessage),
     _ >> [id -:> auto_play_button, innerText <:+ AutoMessage],
     _ >> [id -:> move_button, innerText <:+ MoveMessage, disabled <:- false].

auto_play_message(Prefix, AutoMessage, MoveMessage) :-
    get_number_of_players(NOP),
    !,
       (NOP = 2
           -> Suffix = "Player Two"
         ;
          NOP = 3
           -> Suffix = "Players Two and Three"
         ;
          NOP = 4
           -> Suffix = "Players Two, Three, and Four"
         ),
         append(Prefix, Tail, AutoMessage),
         append(" Auto Play for ", Suffix, Tail),
         append("Move " , Suffix, MoveMessage).
auto_play_message(Prefix, AutoMessage, MoveMessage) :-
    append(Prefix, " Auto Play", AutoMessage),
    MoveMessage = "Move Player(s)".

toggle_debugging :-
    use_debugging(true)
      -> disable_debugging
    ;
    enable_debugging.

toggle_debugging_and_update_button :-
    toggle_debugging,
    update_debugging_button.

update_debugging_button :-
    dom_window(_W) >+> document :> D,
    D >*> getElementsByTagName(console, C),
    C >+> style :> Style,
    (use_debugging(true)
      -> _ >> [id -:> debug_button, innerText <:+ "Disable Debugging"],
         Style >*> setProperty(display, block)
    ;
    true
      -> _ >> [id -:> debug_button, innerText <:+ "Enable Debugging"],
         Style >*> setProperty(display, none)
    ).


 % clientX and clientY are coordinates within the containing HTMLCanvasElement
 % It appears that the rendering coordinates (e.g. moveTo(RX, RY)) are coordinates
 % within the containing HTMLCanvasElement minus the canvas offset.
 % RX = clientX - offsetX.

select(Event) :-
    increment_interaction_counter,
    once(Event >> [pageX +:> PageX, pageY +:> PageY]),
    dom_release_object(Event),
    select1(PageX, PageY, display).

:- dynamic(unbound_action/0).

select1(PageX, PageY, UI) :-
    indicate_waiting(wait),
    setup_select1(PageX, PageY, X, Y),
    process_select(X, Y, Action),
    process_unbound_action(Action),
    (Action = skip(_)
      -> writeln(Action)
    ;
     complete_select(UI),
     (use_auto_play(true)
      -> agent_select_delay
     ;
      true
     )
    ),
    indicate_waiting(initial),
    !.
select1(PageX, PageY, UI) :-
    indicate_waiting(initial),
    get_game_phase(rebuild),
    get_replacements([]),
    get_shaped_positions([])
      -> writeln(retrying_failed(select1(PageX, PageY, UI))),
         find_shaped_locations, % 'reset' the shaped locations
         \+ get_shaped_positions([]),
         select1(PageX, PageY, UI)
    ;
    writeln(failed(select1(PageX, PageY, UI))),
    !,
    fail.

indicate_waiting(wait) :-
    once(Div >> [id -:> waiting]),
    toggle_dom_element_class(Div, loader, add).
indicate_waiting(initial) :-
    once(Div >> [id -:> waiting]),
    toggle_dom_element_class(Div, loader, remove).

complete_select(UI) :-
    update_game_phase,
    get_game_phase(Phase),
    writeln(game_phase(Phase)),
    yield,
    update_selection_marker,
    ((Phase = build;Phase = transform),
     get_turn(1)
      -> increment_round
    ;
    Phase = rebuild,
    get_shaped_positions(_, incomplete)
      -> (get_location_replacements(_, [_|_])
          -> findall(RL, get_location_replacements(RL, [_|_]), RLs),
             get_turn(Turn),
             get_hand(Turn, Hand),
             find_replacements(Hand, RLs),
             writeln(complete_select(recalc_location_replacements, RLs))
        ;
        get_shaped_positions(RLs, incomplete),
        get_turn(Turn),
        get_hand(Turn, Hand),
        find_replacements(Hand, RLs),
        writeln(complete_select(recalc_incomplete_shaped_locations, Hand, RLs))
        )
    ;
    true
    ),
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

process_select(X, Y, Action) :-
    fail_save(select_click(X, Y, Click))
     -> fail_save(apply_click(Click, Action))
    ;
    Action = skip(no_selectable_item).

process_unbound_action(Action) :-
    Action \= unbound_action
      -> retractall(unbound_action) % clear previous unbound_action, if any.
    ;
    retract(unbound_action) % check for immediately previous unbound_action.
      -> writeln(second_unbound_action),
         fail % if there were two unbound actions in a row, then report and fail.
    ;
    Action = unbound_action,
    writeln(Action),
    asserta(unbound_action).

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
%      -> on_click_legal_location(BX, BY, _Action)
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
%          -> on_click_select_replace_tile(ID, Action)
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
%      -> on_click_selected_tile_rotate(ID, Action)
%    ;
%    deselect_tile(OldID, Ctx),
%    on_click_active_hand_tile_select(ID, Action)
%    ).

current_selected_tile(ID) :-
    get_selected_tile_id(ID)
    ;
    ID = none.

deselect_tile(ID, Ctx) :-
    ID \= none
      -> restore_tile_original_colors(ID),
         set_selected_tile_id(none),
         get_shaped_positions(OldLocations),
         clear_location_views(OldLocations, Ctx),
         draw_all_tile(ID, Ctx)
     ;
     true.
/*
Possible set selected hand tile actions:
    - if selected hand tile selectable then record that tile as selected to-be-placed-in-board
      and display its available target locations.
*/
on_click_active_hand_tile_select(ID, Action) :-
    hand_tile_selectable(ID)
      -> Action = select_hand_tile(ID),
         current_selected_tile(OldID),
         once(_ >> [id -:> canvas, getContext('2d') *:> Ctx]),
         deselect_tile(OldID, Ctx),
         set_selected_tile_id(ID),
         draw_all_tile(ID, Ctx),
         find_legal_with_rotation_locations(ID),
         find_legal_locations(ID),
         draw_locations(Ctx)
    ;
    Action = skip(select_unselectable(ID)).

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
on_click_select_replace_tile(ID, Action) :-
    \+ tile_in_replacements(ID)
      -> Action = skip(select_nonreplacement(ID)) % skip this selection attempt
    ;
    current_selected_tile(OldID),
    (OldID = ID
      -> on_click_selected_tile_rotate(ID, Action)
    ;
    Action = select_replacement(ID),
    once(_ >> [id -:> canvas, getContext('2d') *:> Ctx]),
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
on_click_legal_location(BX, BY, Action) :-
    writeln(on_click_legal_location(BX, BY, Action)),
    get_selected_tile_id(Tile),
    set_selected_tile_id(none),
    once(_ >> [id -:> canvas, getContext('2d') *:> Ctx]),
    get_game_phase(Phase),
    (Phase = build
      -> Action = move_hand_tile_to_board(Tile, BX, BY),
         set_game_phase_status(closed),
         increment_turn(_),
         place_tile_on_board_and_draw(Ctx, Tile, BX, BY)
%         ,
%         score_delay(Tile)
    ;
     Phase = rebuild
       -> Action = move_hand_tile_to_board(Tile, BX, BY),
          get_replacements(OldReplacements),
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
                 -> (find_orphans(Orphans),
                     [_|_] = Orphans
                      -> increment_turn(NextTurn),
                         process_orphans(Orphans),
                         (AfterResolutionTurn = none
                           -> set_turn_after_resolution(NextTurn)
                         ;
                         true
                         )
                    ;
                     AfterResolutionTurn = none
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
          Replacements = [],
          get_shaped_positions([RLH|RLT], incomplete)
            -> find_replacements(Hand, [RLH|RLT]),
               writeln(rebuild(recalc_replacements, Hand, [RLH|RLT]))
          ;
          true
          )
    ;
     Phase = replace
      -> Action = move_board_tile(Tile, BX, BY),
         get_replacements(OldReplacements),
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
    Action = skip(legal_location_click_error(Tile, BX, BY, Phase))
    ),
    writeln(finished(on_click_legal_location(BX, BY, Action))).

% process_mismatches sets up another hand, shaped locations,
% and replacement tiles - continuing the resolution sequence.
% The phase will be updated to 'replace' by update_game_phase.

process_mismatches :-
    get_mismatches(Mismatches),
    writeln(process_mismatches(Mismatches)),
    set_mismatches([]),
    create_mismatch_shaped_locations(Mismatches),
    setup_replacements(Mismatches),
    draw_game_tiles.

process_orphans(Orphans) :-
    setup_orphans(Orphans),
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
      -> on_click_selected_tile_rotate(ID, _Action)
    ;
    true % writeln(not_active)
    ).

on_click_selected_tile_rotate(ID, rotate(ID)) :-
    once(_ >> [id -:> canvas, getContext('2d') *:> Ctx]),
    initialize_tile_original_colors(ID),
    tile_rotate_right(ID),
    draw_all_tile(ID, Ctx),
    get_shaped_positions(OldLocations),
    clear_location_views(OldLocations, Ctx),
    find_legal_locations(ID),
    draw_locations(Ctx).

initialize_tile_original_colors(ID) :-
    get_tile_original_colors(ID, none)
      -> get_tile_colors(ID, OriginalColors),
         set_tile_original_colors(ID, OriginalColors)
    ;
    true.

restore_tile_original_colors(ID) :-
    get_tile_original_colors(ID, OriginalColors),
    OriginalColors \= none
      -> set_tile_original_colors(ID, none),
         set_colors(ID, OriginalColors),
         writeln(restored(ID, OriginalColors))
    ;
    writeln(no_restore(ID)).

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
%    on_click_transform_edge(Tile, Edge, _Action).

on_click_transform_edge(Tile, Edge, Action) :-
    get_tile_colors(Tile, Colors),
    nth0(Edge, Colors, Color),
    get_turn(PlayerColor),
    (PlayerColor = Color
      -> Action = skip(current_player_edge(Tile, Edge)) % The edge is the current player's color (and thus cannot be *changed* to the current player's color). Ignore this selection click.
    ;
     writeln(transform_edge(Edge, Color)),
     edge_neighbor_tile(Tile, Edge, NeighborTile)
      -> (get_selected_edge(Tile, NeighborTile)
           -> Action = skip(reserved_edge(Tile, NeighborTile)) % these tiles were placed by previous turn/player. Ignore this selection click.
         ;
          Action = transform(Edge, NeighborTile),
          set_game_phase_status(closed),
          create_transform_shaped_locations(Tile, Edge, NeighborTile),
          set_selected_edge(Tile, NeighborTile),
          setup_replacements([Tile-PlayerColor, NeighborTile-PlayerColor]),
          draw_game_tiles
         )
    ;
     Action = skip(no_neighboring_edge_tile(Tile, Edge)) % There is no neighboring tile for this edge. Ignore this selection click.
    ).

setup_replacements(TilesForHand) :-
    get_shaped_positions(ShapedLocations),
    get_board(BoardTiles),
    find_replacements(BoardTiles, ShapedLocations),
    findall(X, member(X-_, TilesForHand), Xs),
    place_tiles_in_hand(Xs),
    layout_hands.

setup_orphans(Orphans) :-
    place_tiles_in_hand(Orphans),
    find_shaped_locations,
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
    draw_all_tiles_no_center(TileIDs, Ctx, W, H).

draw_locations(Ctx) :-
    get_legal_positions(LegalPositions),
    get_legal_positions_with_rotation(LegalPositionsWithRotation),
    draw_legal_moves(LegalPositions, LegalPositionsWithRotation, Ctx).

% Tile is replacing a tile that was moved out of the
% board to a hand (due to a transform selection or
% to a mismatch).
% Tile is in Replacements.

move_tile_on_board_and_draw(Ctx, Tile, X, Y) :-
    gc,
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
    center_board,
    fail_save(draw_game_tiles(Ctx)).

place_tile_on_board_and_draw(Ctx, Tile, X, Y) :-
    gc,
    writeln(place_tile_on_board_and_draw(Tile, X, Y)),
%    statistics,
    wam_duration(Start),

%    get_tile_display_x(Tile, OldDisplayX),
%    get_tile_display_y(Tile, OldDisplayY),
%    get_tile_size(Tile, OldSize),

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

%    get_tile_display_x(Tile, NewDisplayX),
%    get_tile_display_y(Tile, NewDisplayY),
%    get_tile_size(Tile, NewSize),
%
%    get_tile_colors(Tile, AbstractColors),
%    abstract_colors(AbstractColors, Colors),

%    draw_tile_animation(Ctx, Colors, OldDisplayX>OldDisplayY+OldSize, NewDisplayX>NewDisplayY+NewSize),
%    statistics,

    wam_duration(Mark3),
    center_board,
    fail_save(draw_game_tiles(Ctx)),
%    draw_game_tiles(Ctx),
    get_interaction_counter(InteractionCounter),
    reposition_board_loop_delay(InteractionCounter),

%    writeln(place_tile_on_board_and_draw(finished(reposition_board_loop_delay(InteractionCounter)))),
    wam_duration(Mark4),
    incremental_find_shaped_locations(Tile),
%    writeln(place_tile_on_board_and_draw(finished(incremental_find_shaped_locations(Tile)))),

    wam_duration(End),
    !,
    gc,
    display_spans([Start, Mark1,Mark2, Mark3,  Mark4, End], place_tile_on_board_and_draw),
    writeln(place_tile_on_board_and_draw(finished)).


draw_tile_animation(_Ctx, _Colors, End, End) :-
    !.
draw_tile_animation(Ctx, Colors, Start, End) :-
%    gc,
    writeln(draw_tile_animation(Start)),
    fail_save_dta(Ctx, Colors, Start, End, Next),
    yield,
    draw_tile_animation(Ctx, Colors, Next, End).


fail_save_dta(Ctx, Colors, Start, End, Next) :-
    once_dta(Ctx, Colors, Start, End, Next),
    asserta(fail_saved(draw_tile_animation1(Next))),
%    writeln(asserted(Next)),
    fail.
fail_save_dta(_Ctx, _Colors, _Start, _End, Next) :-
    retract(fail_saved(draw_tile_animation1(Next))),
    writeln(retracted(Next)).

once_dta(Ctx, Colors, Start, End, Next) :-
    draw_tile_animation1(Ctx, Colors, Start, End, Next),
    !.

draw_tile_animation1(Ctx, Colors, Start, End, Next) :-
    draw_game_tiles(Ctx),
    next_position(Start, End, Next),
    draw_tile(Ctx, Colors, Next).

draw_tile(Ctx, Colors, X>Y+Size) :-
    draw_tile(Ctx, Colors, Size, X, Y).

next_position(TX>TY+TS, TargetTX>TargetTY+TargetS, FinalTX>FinalTY+FinalSize) :-
    next_value(TX, TargetTX, FinalTX),
    next_value(TY, TargetTY, FinalTY),
    next_value(TS, TargetS, FinalSize).

next_value(T, Target, Final) :-
    New is (T * 1 + Target) / 2,
    (abs(T - Target) < 1
       -> Final = Target
    ; Final = New
    ).


reposition_board_loop_delay(InteractionCounter) :-
%    true.
    yield,
    get_interaction_counter(CurrentCounter),
    (InteractionCounter == CurrentCounter
      -> reposition_board_loop(InteractionCounter)
    ;
    true).
%    eval_javascript("setTimeout(() => proscriptls('tiles:reposition_board_loop'), 30);").

reposition_board_loop(InteractionCounter) :-
    reposition_board_toward_target_translation,
    !,
    fail_save(draw_game_tiles_and_locations),
    gc,
%    writeln(reposition_board_loop),
%    statistics,
    reposition_board_loop_delay(InteractionCounter).
reposition_board_loop(_InteractionCounter) :-
    true. % calculate_and_display_score.

score_delay :-
    score_delay(0).

score_delay(Tile) :-
%    writeln(score_delay(Tile)),
%    yield,
    indicate_waiting(wait),
    number_codes(Tile, TileCodes),
    append_lists(["setTimeout(() => proscriptls('tiles:score_delay1(", TileCodes, ")'), 0);"], MsgCodes),
    eval_javascript(MsgCodes).

score_delay1(Tile) :-
    indicate_waiting(wait),
    get_turn(Turn),
    get_round(Round),
    (get_game_phase(build)
      -> incremental_score(Tile, Score),
         add_totals(Round/Turn, Score)
    ;
     get_game_phase(rebuild),
     get_hand(Turn, [])
      -> score(Score),
         add_totals(Round/Turn, Score)
    ;
     get_game_phase(transform)
      -> score(Score),
         add_totals(Round/Turn, Score)
    ;
     Score = 'score not calculated'
    ),
    get_totals(Totals),
    display_score(Totals),
    indicate_waiting(initial).


calculate_and_display_score :-
    indicate_waiting(wait),
    score(S),
    get_turn(Turn),
    get_round(Round),
    add_totals(Round/Turn, S),
    get_totals(Totals),
    display_score(Totals),
    indicate_waiting(initial).

/*
<table>
<tr><th>1</th><th>2</th></tr>
<tr><td>125</td><td><b>37</b></td></tr>
<tr><td><b>144</b></td><td>40</td></tr>
<tr><td>74</td><td><b>44</b></td></tr>
</table>
*/
display_score(S) :-
    score_html_table(S, TableBodyCodes),
    get_number_of_players(NumberOfPlayers),
    score_table_header_items(1, NumberOfPlayers, HeaderCodes),
    append_lists(["<table><tr><th>Round</th>", HeaderCodes, "</tr>", TableBodyCodes, "</table>"], TableCodes),
    atom_codes(Table, TableCodes),
    (assess_winner(S, Winner)
        -> get_player_color(Winner, Color),
           format(atom(Score), '<b>Winner: <span style="background-color:~w;">~w</span></b><br>~w', [Color, Winner, Table])
    ;
     format(atom(Score), '~w', [Table])
    ),
    atom_codes(Score, ScoreCodes),
    _ >> [id -:> score, innerHTML <:+ ScoreCodes],
    !.

score_table_header_items(Player, Player, Codes) :-
    !,
    score_table_header_item(Player, Codes).
score_table_header_items(Player, NumberOfPlayers, Codes) :-
    score_table_header_item(Player, ItemCodes),
    append(ItemCodes, NextCodes, Codes),
    Next is Player + 1,
    score_table_header_items(Next, NumberOfPlayers, NextCodes).

score_table_header_item(Player, Codes) :-
    get_player_color(Player, Color),
    format(atom(Item), '<th><div style="background-color:~w;">~w</div></th>', [Color, Player]),
    atom_codes(Item, Codes).

score_html_table([], []).
score_html_table([Round/Turn-Score|OtherScores], TableCodes) :-
    score_html_row(Round, Turn, Score, DataCodes),
    append_lists(["<tr>", DataCodes, "</tr>", OtherCodes], TableCodes),
    score_html_table(OtherScores, OtherCodes).

score_html_row(Round, HighlightTurn, Score, RowCodes) :-
    format(atom(RoundItem), '<td>~w</td>', [Round]),
    atom_codes(RoundItem, RoundCodes),
    append(RoundCodes, ScoreCodes, RowCodes),
    sort(Score, SortedScore),
    score_html_row1(SortedScore, HighlightTurn, ScoreCodes).

score_html_row1([], _, []).
score_html_row1([Turn-Value|T], HighlightTurn, RowCodes) :-
    (Value >= 100
      -> format(atom(Item1), '<div class="mosaic-winning">~w</div>', [Value])
    ;
     Item1 = Value
    ),
    (Turn = HighlightTurn
      -> format(atom(Item2), '<div class="mosaic-current">~w</div>', [Item1])
    ;
     Item2 = Item1
    ),
    format(atom(Datum), '<td>~w</td>', [Item2]),
    atom_codes(Datum, DatumCodes),
    append(DatumCodes, OtherCodes, RowCodes),
    score_html_row1(T, HighlightTurn, OtherCodes).


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
    tile_in_active_hand(Tile),
    hand_tile_selectable(Tile),
    \+ get_selected_tile_id(Tile), % not already selected - it should only be reclicked when already selected.
    get_game_phase(Phase),
    (Phase = build;Phase = rebuild).
available_click(click_board_tile(Tile)) :-
    tile_in_board(Tile),
    get_game_phase(replace),
    tile_in_replacements(Tile),
    \+ get_selected_tile_id(Tile). % not already selected - it should only be reclicked when already selected.
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
    indicate_waiting(initial).

agent_select(Click) :-
    indicate_waiting(wait),
    get_turn(Agent),
    agent_player(Agent)
      -> check_replacements(Agent),
         ask_agent(Click),
         apply_click(Click, Action),
         (Action = skip(_)
           -> writeln(Action),
              agent_select_delay
         ;
          complete_select(display),
          agent_select_delay
         )
    ;
    indicate_waiting(initial).

check_replacements(Turn) :-
    get_replacements(R),
    R \= [],
    get_hand(Turn, Hand),
    member(M, R),
    member(M, Hand),
    \+(get_location_replacements(_, LR),
       member(M, LR))
      -> (get_shaped_positions(RLs, incomplete)
           -> find_replacements(Hand, RLs),
              writeln(check_replacements(recalc(Hand, RLs)))
         ;
          set_replacements([]),
          writeln(check_replacements(clear))
         )
    ;
    true.

agent_player(2).
agent_player(3).
agent_player(4).

ask_agent(Click) :-
    available_clicks(Clicks),
    agent(Clicks, Click).

/*
apply_clicks([click_hand_tile(1), click_location(0,0), click_hand_tile(11), click_location(0,1), click_hand_tile(3), click_location(-1,0), click_hand_tile(9), click_location(1,1), click_hand_tile(8), click_location(-1,1), click_hand_tile(12), reclick(12), click_location(1,2), click_hand_tile(7)]).

apply_clicks([click_edge(16,0), click_board_tile(12), click_location(2,1), click_board_tile(14), reclick(14), click_location(2,0), click_hand_tile(4), reclick(4), reclick(4), reclick(4), click_location(3,2), click_hand_tile(16), reclick(16), click_location(3,-1)]).

tiles:apply_clicks([click_hand_tile(1), click_location(0,0), click_hand_tile(11), click_location(0,1), click_hand_tile(2), click_location(-1,0), click_hand_tile(12), reclick(12), click_location(1,1), click_hand_tile(3)]).

%Following clicks build a board with tiles 3&5 mismatched and 7&11 mismatched.
%build board:
tiles:apply_clicks(
[click_hand_tile(1),click_location(0,0),click_hand_tile(12),click_location(0,1),click_hand_tile(2),click_location(-1,0),click_hand_tile(15),
reclick(15),click_hand_tile(9),click_hand_tile(16),click_hand_tile(14),reclick(14),click_hand_tile(10),click_hand_tile(13),click_hand_tile(11),
reclick(11),click_hand_tile(14),click_location(-1,-1),click_hand_tile(3),reclick(3),reclick(3),reclick(3),click_location(-2,-1),
click_hand_tile(15),click_location(-2,-2),click_hand_tile(4),reclick(4),click_location(-3,-2),click_hand_tile(13),reclick(13),
click_location(-3,-3),click_hand_tile(5),click_location(-2,-3),click_hand_tile(10),click_location(-2,-4),click_hand_tile(6),reclick(6),
reclick(6),reclick(6),click_location(-3,-4),click_hand_tile(11),click_hand_tile(9),reclick(9),click_location(-1,-4),
click_hand_tile(7),click_location(-1,-5),click_hand_tile(11),reclick(11),click_location(-2,-5),click_hand_tile(8),click_location(-3,-5),
click_hand_tile(16),reclick(16),click_location(-4,-5)]).

% transform edges.
tiles:apply_clicks([
% edge transform: player 1
click_edge(15,3),
click_board_tile(1),click_location(-3,-2),
click_board_tile(3),reclick(3),reclick(3),click_location(-2,-2),
click_hand_tile(4),reclick(4),click_location(-2,-1),
click_hand_tile(15),reclick(15),click_location(-2,0),


% replace mismatch: player 2
click_hand_tile(12),click_location(-1,1),

% edge transform: player 2
click_edge(1,0),
click_board_tile(4),
click_board_tile(3),
click_board_tile(11),
click_board_tile(12),reclick(12),click_location(-3,-3),
click_board_tile(4),reclick(4),
click_board_tile(3),reclick(3),
click_board_tile(4),click_location(-3,-2),
click_hand_tile(13),reclick(13),reclick(13),reclick(13),click_location(-2,-1),
click_hand_tile(1),click_location(0,-5)]
).

% transform edges, without extra tile selections and rotations.
tiles:apply_clicks([
% edge transform: player 1
click_edge(15,3),
click_board_tile(1),click_location(-3,-2),
click_board_tile(3),reclick(3),reclick(3),click_location(-2,-2),
click_hand_tile(4),reclick(4),click_location(-2,-1),
click_hand_tile(15),reclick(15),click_location(-2,0),

% replace mismatch: player 2
click_hand_tile(12),click_location(-1,1),

% edge transform: player 2
click_edge(1,0),
click_board_tile(12),reclick(12),click_location(-3,-3),
click_board_tile(4),click_location(-3,-2),
click_hand_tile(13),reclick(13),reclick(13),reclick(13),click_location(-2,-1),
click_hand_tile(1),click_location(0,-5)]
).

*/

apply_clicks([]).
apply_clicks([H|T]) :-
    apply_click(H, _Action),
    apply_clicks1(T).

apply_clicks1([]) :-
    complete_select(display).
apply_clicks1([H|T]) :-
    complete_select(no_display),
    apply_click(H, _Action),
    apply_clicks1(T).

apply_click(Click, Action) :-
    get_turn(Turn),
    get_game_phase(Phase),
    writeln(step(Turn, Phase, Click)),
    assertz(step(Turn, Phase, Click)),
    apply_click1(Click, Action),
    !,
    gc,
    writeln(finished(apply_click(Click, Action))).

apply_click1(reclick(Tile), Action) :-
    on_click_selected_tile_rotate(Tile, Action).
apply_click1(click_hand_tile(Tile), Action) :-
    on_click_active_hand_tile_select(Tile, Action).
apply_click1(click_board_tile(Tile), Action) :-
    on_click_select_replace_tile(Tile, Action).
apply_click1(click_edge(Tile, Edge), Action) :-
    on_click_transform_edge(Tile, Edge, Action).
apply_click1(click_location(BX,BY), Action) :-
    once(get_location_at_grid_point(_, BX, BY)),
    on_click_legal_location(BX, BY, Action).

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
    point_in_legal_location(BX, BY, X, Y),
    !. % there is at most one Location containing X/Y.

get_location_at_grid_point(Location, BX, BY) :-
    get_location_grid_x(Location, BX),
    get_location_grid_y(Location, BY).

steps(S) :-
    findall(step(Turn, Phase, Click), step(Turn, Phase, Click), S).

step_clicks([], []).
step_clicks([step(_, _, Click)|T], [Click|TC]) :-
    step_clicks(T, TC).

apply_steps(S) :-
    step_clicks(S, C),
    apply_clicks(C).

