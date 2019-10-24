:- module(tiles, [select_test/0, rotate_test/0]).

:- use_module('../proscriptls_sdk/library/object'). % for >>/2.
:- use_module('../proscriptls_sdk/library/data_predicates').
:- use_module(library).
:- use_module(model_basics).
:- use_module(view_basics).
:- use_module(tile_model).
:- use_module(game_model_tiles).
:- use_module(tile_view).

:- dynamic(is_selected/1).

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
    get_number_of_players(NumberOfPlayers),
    initial_hands_expanded(NumberOfPlayers, Hands),
    setup_hands(Hands, TileIDs),
    draw_all_tiles(TileIDs, Ctx, W, H).

clear_select_test :-
    get_canvas_width(W),
    get_canvas_height(H),
    get_context(Ctx),
    _Canvas >> [id -:> canvas,
        removeEventListener(click, [object-E]^select(E))],
    Ctx >*> clearRect(0, 0, W, H),
    retractall(is_selected(_)).

rotate_test :-
    setup_game_data,
    clear_tests,
    _TestLabel >> [id -:> current_test, innerHTML <:+ "<h2>Active: Rotate Test</h2>"],
    get_canvas_width(W),
    get_canvas_height(H),
    get_context(Ctx),
    _Canvas >> [id -:> canvas,
        addEventListener(click, [object-E]^rotate(E))],
    get_number_of_players(NumberOfPlayers),
    initial_hands_expanded(NumberOfPlayers, Hands),
    setup_hands(Hands, TileIDs),
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

%point_in_tile(ID, MX, MY) :-
%    tile_x(ID, TX),
%    tile_y(ID, TY),
%    tile_size(ID, Size),
%    in_square(MX, MY, TX, TY, Size).

in_square(X, Y, Left, Top, Size) :-
    in_interval(X, Left, Left+Size),
    in_interval(Y, Top, Top+Size).

in_interval(V, Low, High) :-
    V >= Low,
    V =< High.

point_in_board_position(BX, BY, X, Y) :-
    get_board_tile_size(Size),
    board_position_top_left_coordinates(BX, BY, BCX, BCY),
    in_square(X, Y, BCX, BCY, Size).

on_click_tile(ID, _X, _Y) :-
    %writeln(click(ID, X, Y)),
    (tile_in_active_hand(ID)
      -> on_click_active_hand_tile(ID)
    ;
    true % writeln(not_active)
    ).

on_click_active_hand_tile(ID) :-
    _ >> [id -:> canvas, getContext('2d') *:> Ctx],
    (retract(is_selected(OldID))
      -> draw_all_tile(OldID, Ctx) % de-select OldID
    ;
    true
    ),
    asserta(is_selected(ID)),
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


setup_hands([], []).
setup_hands([_+HandTiles|T], IDs) :-
    %writeln(setting_up_hand(HandTiles)),
    setup_hand(HandTiles, IDs, IDTail),
    setup_hands(T, IDTail).

setup_hand([], Tail, Tail).
setup_hand([x(X, Y, Size,GridX,GridY)-ID|T], [ID|OtherIDs], IDTail) :-
    %writeln(setup_hand_tile(x(H,GridX,GridY)-ID)),
    update_grid_x(ID, _, GridX),
    update_grid_y(ID, _, GridY),
    %assert_data(H, ID),
    create_tile_view(ID, X, Y, Size),
    setup_hand(T, OtherIDs, IDTail).

hand_origin(1).

initial_hands_expanded(NumberOfPlayers, ExpandedHands) :-
    initial_hands(NumberOfPlayers, Hands),
    expand_hands(Hands, ExpandedHands).

expand_hands([], []).
expand_hands([H|T], [EH|ET]) :-
    expand_hand(H, EH),
    expand_hands(T, ET).

expand_hand(ID+BriefTiles, ID+ExpandedTiles) :-
    get_hand_tile_size(Size),
    expand_brief_tiles(BriefTiles, Size, ExpandedTiles).

expand_brief_tiles([], _, []).
expand_brief_tiles([H|T], Size, [EH|ET]) :-
    expand_brief_tile(H, Size, EH),
    expand_brief_tiles(T, Size, ET).

% [x, y,bx,by,size,colors,container]
expand_brief_tile(t(BoardX, BoardY, TileID),
        Size,
        x(X, Y, Size, BoardX, BoardY)-TileID) :- % make the TileID and ModelID the same.
    X is BoardX * Size,
    Y is BoardY * Size.

abstract_colors([], []).
abstract_colors([AH|AT], [CH|CT]) :-
    get_player_color(AH, CH),
    abstract_colors(AT, CT).

abstract_color(a, red).
abstract_color(b, green).
abstract_color(c, blue).
abstract_color(d, yellow).

/*
Fill in gridX and gridY values for the tile models created by game_model_tiles:build_tiles/0
such that the tiles for a hand are grouped together in a vertical rectangle 2 columns wide and 4
rows deep and these two hand rectangles are placed one above the other with a small space
separating them.
*/
initial_hands(2, [1+Player1Tiles, 2+Player2Tiles]) :-
    hand_origin(Origin1),
    Origin2 is Origin1 + 5,
    get_hands([ModelHand1, ModelHand2]),
    place_hand(ModelHand1, 1, Origin1, 0, 4, Player1Tiles),
    length(Player1Tiles, Player1TilesLength),
    place_hand(ModelHand2, 1, Origin2, Player1TilesLength, 4, Player2Tiles).

% place_hand(Hand, BaseCol, BaseRow, PlacedSoFar, MaxColumns, MaxRows, PlacedHand).
place_hand(AbstractHand, BaseCol, BaseRow, PlacedSoFar, MaxRows, PlacedHand) :-
    place_hand(AbstractHand, BaseCol, BaseRow, PlacedSoFar, PlacedSoFar, MaxRows, PlacedHand).

place_hand([], _BaseCol, _BaseRow, _Counter, _InitialCounter, _MaxRows, []).
place_hand([H|T], BaseCol, BaseRow, Counter, InitialCounter, MaxRows, [HP|TP]) :-
    place_hand1(H, BaseCol, BaseRow, Counter, InitialCounter, MaxRows, HP),
    CounterNEXT is Counter + 1,
    place_hand(T, BaseCol, BaseRow, CounterNEXT, InitialCounter, MaxRows, TP).

place_hand1(H, BaseCol, BaseRow, Counter, InitialCounter, MaxRows, t(Col, Row, H)) :-
    RowIncrement is (Counter-InitialCounter) mod MaxRows,
    ColIncrement is (Counter-InitialCounter) // MaxRows,
    Col is BaseCol + ColIncrement,
    Row is BaseRow + RowIncrement.

% (X > Y) is a point (X,Y).
% Web API method arguments of type number or integer accept arithmetic
% expressions; e.g. (1 + 0.5 * 50).

draw_tile(Ctx, Tile) :-
    get_tile_display_x(Tile, X),
    get_tile_display_y(Tile, Y),
    get_tile_size(Tile, Size),
    Corners = [X > Y,X + Size > Y,X + Size > Y + Size, X > Y + Size],
    Center = (X + 0.5 * Size > Y + 0.5 * Size),
    get_tile_colors(Tile, AbstractColors),
    abstract_colors(AbstractColors, Colors),
    draw_triangles(Corners, Colors, Center, Ctx),
    get_tile_grid_x(Tile, BX),
    get_tile_grid_y(Tile, BY),
    board_hash_key_coords(BX, BY, Text),
    Ctx >> [
        save,
        fillStyle <:+ '#000',
        fillText(Text, X+5, Y+10),
        restore
    ].


draw_triangles([P1, P2|OtherCorners], [Color1|OtherColors], Center, Ctx) :-
   draw_triangle(P1, P2, Color1, Center, Ctx),
   draw_triangles1([P2|OtherCorners], OtherColors, P1, Center, Ctx).

draw_triangles1([P1], [Color], P2, Center, Ctx) :-
   draw_triangle(P1, P2, Color, Center, Ctx).
draw_triangles1([P1, P2|OtherCorners], [Color1|OtherColors], FirstP, Center, Ctx) :-
   draw_triangle(P1, P2, Color1, Center, Ctx),
   draw_triangles1([P2|OtherCorners], OtherColors, FirstP, Center, Ctx).

draw_triangle(P1x > P1y, P2x > P2y, Color, CenterX > CenterY, Ctx) :-
    Ctx >> [
        beginPath,
        moveTo(P1x, P1y),
        lineTo(P2x, P2y),
        lineTo(CenterX, CenterY),
        closePath,

        save,
        fillStyle <:+ Color,
        fill,
        stroke,
        restore
    ].

draw_all_tiles(AllTiles, Ctx, CW, CH) :-
    center_board,
    Ctx >> [
        fillStyle <:+ '#999',
        fillRect(0, 0, CW, CH)
    ],
    draw_all_tiles1(AllTiles, Ctx).

draw_all_tiles1([], _).
draw_all_tiles1([H|T], Ctx) :-
    writeln(draw_all_tile(H)),
    draw_all_tile(H, Ctx),
    draw_all_tiles1(T, Ctx).

draw_all_tile(Tile, Ctx) :-
    (tile_in_inactive_hand(Tile) -> GlobalAlpha = 0.3; GlobalAlpha = 1),
    Ctx >> [
        save,
        globalAlpha <:+ GlobalAlpha
    ],
    draw_tile(Ctx, Tile),
    Ctx >*> restore,
    (is_selected(Tile)
        -> draw_selected_tile_mark(Tile, Ctx)
     ;
     true
    ),
    draw_replacements(Tile, Ctx).

draw_replacements(Tile, Ctx) :-
    (game_replacements(Rs),
     member(Tile, Rs)
       -> draw_replacement_tile_mark(Tile, Ctx)
    ;
    true
    ).

center_board.

draw_selected_tile_mark(Tile, Ctx) :-
    get_tile_display_x(Tile, X),
    get_tile_display_y(Tile, Y),
    get_tile_size(Tile, Size),

    MidX is X + (Size / 2),
    MidY is Y + (Size / 2),
    Adjust is Size / 4,

	VerticalTopX = MidX,
	VerticalTopY is MidY - Adjust,
	VerticalBottomX = MidX,
	VerticalBottomY is MidY + Adjust,
	HorizontalLeftX is MidX-Adjust,
	HorizontalLeftY = MidY,
	HorizontalRightX is MidX+Adjust,
	HorizontalRightY = MidY,

	get_turn(GT),
	get_highlight_color(GT, Color),
	%highlight_color(GT, Color),

	Ctx >> [
	    save,

	    lineWidth <:+ 3,
	    strokeStyle <:+ Color,
	    beginPath,
	    moveTo(VerticalTopX, VerticalTopY),
	    lineTo(VerticalBottomX, VerticalBottomY),
	    closePath,
	    stroke,

	    beginPath,
	    moveTo(HorizontalLeftX, HorizontalLeftY),
	    lineTo(HorizontalRightX, HorizontalRightY),
	    closePath,
	    stroke,

	    restore
	].

draw_replacement_tile_mark(Tile, Ctx) :-
    get_tile_display_x(Tile, X),
    get_tile_display_y(Tile, Y),
    get_tile_size(Tile, Size),

    MidX is X + (Size / 2),
    MidY is Y + (Size / 2),
    Adjust is Size / 4,

	get_turn(GT),
	get_highlight_color(GT, Color),
    %highlight_color(GT, Color),

	Ctx >> [
	    save,
	    lineWidth <:+ 3,
	    strokeStyle <:+ Color,
	    beginPath,
	    arc(MidX, MidY, Adjust, 0, 2*pi),
	    closePath,
	    stroke,
	    restore
	].

draw_legal_moves(LegalPositions, LegalPositionsWithRotation, Ctx) :-
	get_turn(GT),
	highlight_color(GT, Color),

    Ctx >> [
        save,
        beginPath,
        fillStyle <:+ Color,
        strokeStyle <:+ Color,
        lineWidth <:+ 3,
        globalAlpha <:+ 0.8
    ],

    get_board_tile_size(TileSize),

    draw_legal_positions_with_rotations(LegalPositionsWithRotation, Ctx, TileSize),

    draw_legal_positions(LegalPositions, Ctx, TileSize),

    Ctx >*> restore.

draw_legal_positions_with_rotations([], _, _).
draw_legal_positions_with_rotations([H|T], Ctx, TileSize) :-
    legal_position_b(H, B),
    get_top_left_board_tile_coords(B, TileSize, X > Y),
    Ctx >*> [
        rect(X, Y, TileSize, TileSize),
        stroke
    ],
    draw_legal_positions_with_rotations(T, Ctx, TileSize).

draw_legal_positions([], _, _).
draw_legal_positions([H|T], Ctx, TileSize) :-
    legal_position_b(H, B),
    get_top_left_board_tile_coords(B, TileSize, X > Y),
    Ctx >*> fillRect(X, Y, TileSize, TileSize),
    draw_legal_positions(T, Ctx, TileSize).

get_top_left_board_tile_coords(BX > BY, TileSize, X > Y) :-
    get_board_left(Left),
    game_board_translate(TX > TY),
    get_board_width(W),
    get_board_top(Top),
    get_board_height(H),
    X  is Left + TX + (W / 2) + (BX - 0.5) * TileSize,
    Y  is Top + TY + (H / 2) + (BY - 0.5) * TileSize.

container_id(hand(ID), ID).
container_type(hand(_ID), hand).
container_type(board, board).

%tile_label(BoardX, BoardY, Text) :-
%    number_codes(BoardX, BXCodes),
%    number_codes(BoardY, BYCodes),
%    append_lists(["x", BXCodes, "y", BYCodes], TextCodes),
%    atom_codes(Text, TextCodes).

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

highlight_color(1, '#CCFFCC').
highlight_color(2, '#CCCCFF').

%legal_position_bx(legal_position(BX, _BY), BX).
%legal_position_by(legal_position(_BX, BY), BY).

legal_position_b(T, BX > BY) :-
    legal_position_bx(T, BX),
    legal_position_by(T, BY).
