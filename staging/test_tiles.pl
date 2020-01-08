:- module(test_tiles, [select_test/0, rotate_test/0, view_basics_test/0,
    game_model_tiles_test/0, tile_view_test/0, game_view_tiles_test/0, move_to_board_test/0,
    reposition_board_loop/0]).

:- use_module('../proscriptls_sdk/library/object'). % for >>/2.
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
:- use_module(tiles).

dummy_reference :-
    dummy_reference,
    rotate(_),
    move_to_board(_).

clear_tests :-
    clear_select_test,
    clear_rotate_test,
    clear_move_to_board_test,
    clear_view_basics_test,
    clear_game_model_tiles_test,
    clear_tile_view_test.

view_basics_test :-
    setup_game_data,
    clear_tests,
    view_basics_values(V),
    display_key_values("View Basics", V).

clear_view_basics_test :-
    clear_display_key_values.

game_model_tiles_test :-
    setup_game_data,
    clear_tests,
    game_model_tiles_values(V),
    display_key_values("Game Model Tiles", V).

clear_game_model_tiles_test :-
    clear_display_key_values.

tile_view_test :-
    setup_game_data,
    clear_tests,
    create_tile_view(1, 10, 15, 55),
    tile_view_values(1, V),
    display_key_values("Tile View", V).

clear_tile_view_test :-
    clear_display_key_values.

game_view_tiles_test :-
    setup_game_data,
    clear_tests,
    create_tile_view(1, 10, 15, 55),
    game_view_tiles_values(1, V),
    display_key_values("Game View Tiles", V).

game_view_tiles_test :-
    clear_display_key_values.

clear_display_key_values :-
    _Elements >> [id -:> display_elements, innerHTML <:+ ""].

display_key_values(LabelCodes, V) :-
    append_lists(["<h2>Active: ", LabelCodes, " Test</h2>"], MessageCodes),
    _TestLabel >> [id -:> current_test, innerHTML <:+ MessageCodes],
    Elements >-> id :> display_elements,
    create_dom_element(ul, ULElement),
    append_dom_node_child(Elements, ULElement),
    display_key_values1(V, ULElement).


display_key_values1([], _).
display_key_values1([H|T], Elements) :-
    display_key_value(H, Elements),
    display_key_values1(T, Elements).

display_key_value(Key - Value, Elements) :-
    format(atom(HTML), '<li>~w: ~w</li>\n', [Key, Value]),
    atom_codes(HTML, HTMLCodes),
    Elements >*> insertAdjacentHTML(beforeEnd, HTMLCodes).

select_test :-
    clear_tests,
    _TestLabel >> [id -:> current_test, innerHTML <:+ "<h2>Active: Select Test</h2>"],
    start_mosaic_game.

clear_select_test :-
    clear_mosaic_game.

rotate_test :-
    clear_tests,
    setup_game_data,
    _TestLabel >> [id -:> current_test, innerHTML <:+ "<h2>Active: Rotate Test</h2>"],
    get_canvas_width(W),
    get_canvas_height(H),
    get_context(Ctx),
    _Canvas >> [id -:> canvas,
        addEventListener(click, [object-E]^rotate(E))],
    get_tiles(TileIDs),
    draw_all_tiles(TileIDs, Ctx, W, H).

clear_rotate_test :-
    get_canvas_width(W),
    get_canvas_height(H),
    get_context(Ctx),
    _Canvas >> [id -:> canvas,
        removeEventListener(click, [object-E]^rotate(E))],
    Ctx >*> clearRect(0, 0, W, H).

move_to_board_test :-
    setup_game_data,
    clear_tests,
    _TestLabel >> [id -:> current_test, innerHTML <:+ "<h2>Active: Select Test</h2>"],
    get_canvas_width(W),
    get_canvas_height(H),
    get_context(Ctx),
    _Canvas >> [id -:> canvas,
        addEventListener(click, [object-E]^move_to_board(E))],
    get_tiles(TileIDs),
    draw_all_tiles(TileIDs, Ctx, W, H).

clear_move_to_board_test :-
    get_canvas_width(W),
    get_canvas_height(H),
    get_context(Ctx),
    _Canvas >> [id -:> canvas,
        removeEventListener(click, [object-E]^move_to_board(E))],
    Ctx >*> clearRect(0, 0, W, H).

rotate(Event) :-
    Event >> [pageX +:> PageX, pageY +:> PageY],
    dom_release_object(Event),
    get_canvas_offset_top(PTop),
    get_canvas_offset_left(PLeft),
    X is PageX - PLeft,
    Y is PageY - PTop,
    % writeln(select(PageX, PageY, PLeft, PTop, X, Y)),
    (point_in_tile(ID, X, Y)
      -> on_click_tile_rotate(ID, X, Y)  % at most one tile contains (X, Y).
    ;
     true
    ).

move_to_board(Event) :-
    Event >> [pageX +:> PageX, pageY +:> PageY],
    dom_release_object(Event),
    get_canvas_offset_top(PTop),
    get_canvas_offset_left(PLeft),
    X is PageX - PLeft,
    Y is PageY - PTop,
    % writeln(select(PageX, PageY, PLeft, PTop, X, Y)),
    (point_in_tile(ID, X, Y)
      -> on_click_tile_move(ID, X, Y)  % at most one tile contains (X, Y).
    ;
     true
    ).

on_click_tile_rotate(ID, _X, _Y) :-
    %writeln(click(ID, X, Y)),
    (tile_in_active_hand(ID)
      -> on_click_active_hand_tile_rotate(ID)
    ;
    true % writeln(not_active)
    ).

on_click_tile_move(ID, _X, _Y) :-
    %writeln(on_click_tile_move(ID, X, Y)),
    (tile_in_active_hand(ID)
      -> on_click_active_hand_tile_move(ID)
    ;
    true % writeln(not_active)
    ).

on_click_active_hand_tile_move(ID) :-
    _ >> [id -:> canvas, getContext('2d') *:> Ctx],
    place_tile_on_board_and_draw(Ctx, ID, 0, 0).
