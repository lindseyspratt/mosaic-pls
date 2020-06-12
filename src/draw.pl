:- module(draw, [draw_all_tiles/4, draw_all_tiles_no_center/4, draw_all_tile/2, draw_legal_moves/3, clear_location_views/2,
    draw_replacements/2, draw_replacement_tile_mark/2, draw_tile/5, abstract_colors/2, draw_label/4]).

:- use_module('../proscriptls_sdk/library/object'). % for >>/2.
:- use_module(model_basics).
:- use_module(tile_model).
:- use_module(game_model_tiles).
:- use_module(view_basics).
:- use_module(tile_view).
:- use_module(game_view_tiles).
:- use_module(location_model).
:- use_module(geometry).

abstract_colors([], []).
abstract_colors([AH|AT], [CH|CT]) :-
    get_player_color(AH, CH),
    abstract_colors(AT, CT).

% (X > Y) is a point (X,Y).
% Web API method arguments of type number or integer accept arithmetic
% expressions; e.g. (1 + 0.5 * 50).

draw_tile(Ctx, Tile) :-
    get_tile_display_x(Tile, X),
    get_tile_display_y(Tile, Y),
    get_tile_size(Tile, Size),
    get_tile_colors(Tile, AbstractColors),
    abstract_colors(AbstractColors, Colors),
    draw_tile(Ctx, Colors, Size, X, Y),
    get_tile_grid_x(Tile, BX),
    get_tile_grid_y(Tile, BY),
    board_hash_key_coords(BX, BY, Text),
    format(atom(TileAtom), '~w', [Tile]),
    Ctx >> [
        save,
        fillStyle <:+ '#000',
        fillText(Text, X+5, Y+10),
        fillText(TileAtom, X+20, Y+20),
        restore
    ],
    !. % several predicates in this clause leave choicepoints (needlessly). This cut removes them.

draw_tile(Ctx, Colors, Size, X, Y) :-
    Corners = [X > Y,X + Size > Y,X + Size > Y + Size, X > Y + Size],
    Center = (X + 0.5 * Size > Y + 0.5 * Size),
    draw_triangles(Corners, Colors, Center, Ctx),
    Ctx >> [
        beginPath,
        moveTo(X, Y),
        lineTo(X + Size, Y),
        lineTo(X + Size, Y + Size),
        lineTo(X, Y + Size),
        closePath,

        save,
        strokeStyle <:+ '#000',
        stroke,
        restore
    ].

draw_triangles([P1, P2|OtherCorners], [Color1|OtherColors], Center, Ctx) :-
   draw_triangle(P1, P2, Color1, Center, Ctx),
   draw_triangles1([P2|OtherCorners], OtherColors, P1, Center, Ctx).

draw_triangles1([P1], [Color], P2, Center, Ctx) :-
   !,
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
        strokeStyle <:+ Color,
        stroke,
        restore
    ].

clear_board_rect(Ctx, X, Y, W, H) :-
    Ctx >> [
        fillStyle <:+ '#999',
        fillRect(X, Y, W, H),
        strokeStyle <:+ '#999',
        stroke
    ].

draw_all_tiles(AllTiles, Ctx, CW, CH) :-
    center_board,
    draw_all_tiles_no_center(AllTiles, Ctx, CW, CH).

draw_all_tiles_no_center(AllTiles, Ctx, CW, CH) :-
    clear_board_rect(Ctx, 0, 0, CW, CH),
    draw_hand_labels(Ctx, CW, CH),
    draw_all_tiles1(AllTiles, Ctx).

draw_all_tiles1([], _).
draw_all_tiles1([H|T], Ctx) :-
    %writeln(draw_all_tile(H)),
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
    (get_selected_tile_id(Tile),
     Tile \= none
        -> draw_selected_tile_mark(Tile, Ctx)
     ;
     true
    ),
    draw_replacements(Tile, Ctx).

draw_hand_labels(Ctx, CW, CH) :-
    get_number_of_players(NOP),
    get_label_height(LabelHeight),
    get_hand_margin(Margin),
    draw_hand_labels(NOP, Ctx, CW, CH, LabelHeight, Margin).

draw_hand_labels(0, _Ctx, _CW, _CH, _LabelHeight, _Margin) :-
    !.
draw_hand_labels(ID, Ctx, CW, CH, LabelHeight, Margin) :-
    ID > 0,
    draw_hand_label(ID, Ctx, CW, CH, LabelHeight, Margin),
    NextID is ID - 1,
    draw_hand_labels(NextID, Ctx, CW, CH, LabelHeight, Margin).

draw_hand_label(1, Ctx, _CW, _CH, LabelHeight, Margin) :-
    draw_label(Ctx, Margin, LabelHeight, 'Player One').
draw_hand_label(2, Ctx, CW, _CH, LabelHeight, Margin) :-
    get_number_of_players(NOP),
    get_hand_margin(Margin),
    get_hand_tile_size(TileSize),
    get_label_width(LabelWidth),
    X is CW - max(LabelWidth, (NOP - 1) * (Margin + TileSize)),
    draw_label(Ctx, X, LabelHeight, 'Player Two').
draw_hand_label(3, Ctx, CW, _CH, LabelHeight, Margin) :-
    get_hand_tile_size(TileSize),
    X is (CW - 8 * (Margin + TileSize)) / 2,
    draw_label(Ctx, X, LabelHeight, 'Player Three').
draw_hand_label(4, Ctx, CW, CH, _LabelHeight, Margin) :-
    get_hand_tile_size(TileSize),
    X is (CW - 8 * (Margin + TileSize)) / 2,
    Y is CH - Margin,
    draw_label(Ctx, X, Y, 'Player Four').

draw_label(Ctx, X, Y, Text) :-
    Ctx >> [
        save,
        font <:+ '18px serif',
        fillStyle <:+ '#000',
        fillText(Text, X, Y),
        restore
    ].

draw_replacements(Tile, Ctx) :-
    (get_replacements(Rs),
     member(Tile, Rs)
       -> draw_replacement_tile_mark(Tile, Ctx)
    ;
    true
    ).

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
	get_highlight_color(GT, Color),
	%highlight_color(GT, Color),

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
    get_location_grid_x(H, BX),
    get_location_grid_y(H, BY),
    get_top_left_board_tile_coords(BX, BY, X, Y),
    Ctx >*> [
        rect(X, Y, TileSize, TileSize),
        stroke
    ],
    draw_legal_positions_with_rotations(T, Ctx, TileSize).

draw_legal_positions([], _, _).
draw_legal_positions([H|T], Ctx, TileSize) :-
    get_location_grid_x(H, BX),
    get_location_grid_y(H, BY),
    get_top_left_board_tile_coords(BX, BY, X, Y),
    Ctx >*> fillRect(X, Y, TileSize, TileSize),
    draw_legal_positions(T, Ctx, TileSize).

%highlight_color(1, '#CCFFCC').
%highlight_color(2, '#CCCCFF').

clear_location_views(Locations, Ctx) :-
    get_board_tile_size(TileSize),
    clear_location_views(Locations, Ctx, TileSize).

clear_location_views([], _, _).
clear_location_views([H|T], Ctx, TileSize) :-
    clear_location_view(H, Ctx, TileSize),
    clear_location_views(T, Ctx, TileSize).

clear_location_view(Location, Ctx, TileSize) :-
    get_location_grid_x(Location, BX),
    get_location_grid_y(Location, BY),
    get_top_left_board_tile_coords(BX, BY, X, Y),
    clear_board_rect(Ctx, X, Y, TileSize, TileSize).

