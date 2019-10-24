:- module(tiles, [select_test/0, rotate_test/0]).

:- use_module('../proscriptls_sdk/library/object'). % for >>/2.
:- use_module('../proscriptls_sdk/library/data_predicates').
:- use_module(library).
:- use_module(model_basics).
:- use_module(view_basics).
:- use_module(tile_model).
:- use_module(game_model_tiles).
:- use_module(tile_view).
:- use_module(game_view_tiles).
:- use_module(draw).

% For tiles the shadow tile structure functor is 'ts'
% and the arguments are 'x', 'y', etc.
% For the game the shadow game structure functor is 'g'
% and the arguments are 'turn', etc.
% (Note that there is currently only one 'game' so the ID is always '1'.)

:- initialization(init).

init :-
    data_predicate_dynamics([
        data_predicates(g, game,[board_translate, replacements]), % e.g. game_board_translate(ID, X>Y)...
        %data_predicates(ts, tile,[x, y,size]), % e.g. tile_x(ID, X), tile_y(ID, Y)...
        data_predicates(lp, legal_position, [bx, by])
    ]).

clear_tests :-
    clear_select_test,
    clear_rotate_test,
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

setup_game_data :-
    init_model_basics(2, 4, [1,2,3,4]),
    init_game_model_tiles, % uses info in model_basics.
    increment_turn(1), % the first increment should move the turn from the initial 0 to 1.
    create_view_basics,
    assert_data(g(1>1, []), 1),
    create_game_view_tiles,
    layout_hands,
    !.

select_test :-
    setup_game_data,
    clear_tests,
    _TestLabel >> [id -:> current_test, innerHTML <:+ "<h2>Active: Select Test</h2>"],
    get_canvas_width(W),
    get_canvas_height(H),
    get_context(Ctx),
    _Canvas >> [id -:> canvas,
        addEventListener(click, [object-E]^select(E))],
    get_tiles(TileIDs),
    draw_all_tiles(TileIDs, Ctx, W, H).

clear_select_test :-
    get_canvas_width(W),
    get_canvas_height(H),
    get_context(Ctx),
    _Canvas >> [id -:> canvas,
        removeEventListener(click, [object-E]^select(E))],
    Ctx >*> clearRect(0, 0, W, H),
    set_selected_tile_id(none).

rotate_test :-
    setup_game_data,
    clear_tests,
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

 % clientX and clientY are coordinates within the containing HTMLCanvasElement
 % It appears that the rendering coordinates (e.g. moveTo(RX, RY)) are coordinates
 % within the containing HTMLCanvasElement minus the canvas offset.
 % RX = clientX - offsetX.

select(Event) :-
    Event >> [pageX +:> PageX, pageY +:> PageY],
    dom_release_object(Event),
    get_canvas_offset_top(PTop),
    get_canvas_offset_left(PLeft),
    X is PageX - PLeft,
    Y is PageY - PTop,
    % writeln(select(PageX, PageY, PLeft, PTop, X, Y)),
    (point_in_tile(ID, X, Y)
      -> on_click_tile(ID, X, Y)  % at most one tile contains (X, Y).
    ;
     legal_position_bx(ID, BX),
     legal_position_by(ID, BY),
     point_in_board_position(BX, BY, X, Y)
      -> true
    ;
     true
    ).

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
     legal_position_bx(ID, BX),
     legal_position_by(ID, BY),
     point_in_board_position(BX, BY, X, Y)
      -> true
    ;
     true
    ).

on_click_tile(ID, _X, _Y) :-
    %writeln(click(ID, X, Y)),
    (tile_in_active_hand(ID)
      -> on_click_active_hand_tile(ID)
    ;
    true % writeln(not_active)
    ).

on_click_active_hand_tile(ID) :-
    _ >> [id -:> canvas, getContext('2d') *:> Ctx],
    (get_selected_tile_id(OldID),
     OldID \= none
      -> set_selected_tile_id(none),
         draw_all_tile(OldID, Ctx) % de-select OldID
    ;
    true
    ),
    set_selected_tile_id(ID),
    draw_all_tile(ID, Ctx).

on_click_tile_rotate(ID, _X, _Y) :-
    %writeln(click(ID, X, Y)),
    (tile_in_active_hand(ID)
      -> on_click_active_hand_tile_rotate(ID)
    ;
    true % writeln(not_active)
    ).

on_click_active_hand_tile_rotate(ID) :-
    _ >> [id -:> canvas, getContext('2d') *:> Ctx],
    tile_rotate_right(ID),
    draw_all_tile(ID, Ctx).
