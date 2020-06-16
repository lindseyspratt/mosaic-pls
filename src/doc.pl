:- module(doc, [setup_doc/0]).

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
:- use_module(letters).

setup_doc :-
    display_title,
    init_model_basics(4, 4, [1,2,3,4]),
    create_view_basics(region_spans),
    create_game_view_tiles,
    display_region_spans,
    display_build,
    display_transform,
    display_shape_1,
    display_build_detail,
    display_transform_detail.

display_title :-
    _ >> [id -:> title_canvas, getContext('2d') *:> Ctx],
    letters:display_letters([m,o,s,a,i,c], Ctx, 20, 100, 100, _).

display_region_spans :-
    _Canvas >> [id -:> region_spans, getContext('2d') *:> Ctx],
    get_player_color(1, Color1),
    get_player_color(2, Color2),
    get_board_tile_size(Size),
    display_region_span_1(Ctx, Color1, Color2, Size, -8, -2),
    display_region_span_2(Ctx, Color1, Color2, Size, -4, -1).

display_region_span_1(Ctx, C1, C2, Size, ShiftX, ShiftY) :-
    display_tiles('region of 2 tiles. 4 points.',
        [0 > 0 - [C1, C1, C2, C2],
         -1 > 0 - [C1, C2, C1, C2]
        ], Ctx, Size, ShiftX, ShiftY, separator).

display_region_span_2(Ctx, C1, C2, Size, ShiftX, ShiftY) :-
    display_tiles('region of 6 tiles. 36 points.',
        [0 > -1 - [C2, C1, C1, C2],
         1 > -1 - [C2, C2, C1, C1],
         0 > 0 - [C1, C2, C1, C2],
         1 > 0 - [C1, C1, C1, C2],
         0 > 1 - [C1, C1, C2, C2],
         1 > 1 - [C1, C2, C2, C1]
        ], Ctx, Size, ShiftX, ShiftY).

display_build :-
    _Canvas >> [id -:> build, getContext('2d') *:> Ctx],
    get_player_color(1, C1),
    get_player_color(2, C2),
    get_board_tile_size(Size),
    display_build_1(Ctx, C1, C2, Size, -10, 0),
    display_build_2(Ctx, C1, C2, Size, -5, 0),
    display_build_3(Ctx, C1, C2, Size, 0, 0).

display_build_1(Ctx, C1, C2, Size, ShiftX, ShiftY) :-
    display_tiles('First tile placed in board by player 1.',
        [0 > 0 - [C2, C1, C1, C1]
        ], Ctx, Size, ShiftX, ShiftY, separator).

display_build_2(Ctx, C1, C2, Size, ShiftX, ShiftY) :-
    display_tiles('Second tile placed in board by player 2.',
        [0 > 0 - [C2, C1, C1, C1],
         0 > -1 - [C2, C2, C2, C2]
        ], Ctx, Size, ShiftX, ShiftY, separator).

display_build_3(Ctx, C1, C2, Size, ShiftX, ShiftY) :-
    display_tiles('Third tile placed in board by player 1.',
        [0 > 0 - [C2, C1, C1, C1],
         0 > -1 - [C2, C2, C2, C2],
         1 > -1 - [C1, C2, C1, C2]
        ], Ctx, Size, ShiftX, ShiftY).

display_transform :-
    _Canvas >> [id -:> transform, getContext('2d') *:> Ctx],
    get_player_color(1, C1),
    get_player_color(2, C2),
    get_board_tile_size(Size),
    display_transform_1(Ctx, C1, C2, Size, -12, -1),
    display_transform_2(Ctx, C1, C2, Size, -7, -1),
    display_transform_3(Ctx, C1, C2, Size, 0, -1),
    display_transform_4(Ctx, C1, C2, Size, 7, -1).

display_transform_1(Ctx, C1, C2, Size, ShiftX, ShiftY) :-
    display_tiles('Transform shared sides of two tiles.',
        [0 > -1 - [C2, C1, C1, C2],
         1 > -1 - [C2, C2, C1, C1],
         2 > -1 - [C2, C1, C2, C2],
         0 > 0 - [C1, C2, C1, C2],
         1 > 0 - [C1, C1, C1, C2],
         2 > 0 - [C2, C2, C2, C1],
         0 > 1 - [C1, C1, C2, C2],
         1 > 1 - [C1, C2, C2, C1]
        ], Ctx, Size, ShiftX, ShiftY, separator),
    get_highlight_color(2, Color),
    display_edge_mark([0> -1 - 3, 0>0 - 1], Color, Ctx, Size, ShiftX, ShiftY).

display_transform_2(Ctx, C1, C2, Size1, ShiftX1, ShiftY1) :-
    display_tiles('Select replacement tiles.',
        [1 > -1 - [C2, C2, C1, C1],
         2 > -1 - [C2, C1, C2, C2],
         1 > 0 - [C1, C1, C1, C2],
         2 > 0 - [C2, C2, C2, C1],
         0 > 1 - [C1, C1, C2, C2],
         1 > 1 - [C1, C2, C2, C1]
        ], Ctx, Size1, ShiftX1, ShiftY1),
    get_highlight_color(2, Color),
    display_replacement_tile_mark(2, -1, Color, Ctx, Size1, ShiftX1, ShiftY1),
    display_replacement_tile_mark(2, 0, Color, Ctx, Size1, ShiftX1, ShiftY1),
    ShiftX2 is ShiftX1 + 4,
    ShiftY2 is ShiftY1,
    Size2 is Size1 * 0.75,
    display_tiles('Hand tiles.',
        [0 > -1 - [C2, C1, C1, C2],
         0 > 0 - [C1, C2, C1, C2]
        ], Ctx, Size2, ShiftX2, ShiftY2, separator).

display_transform_3(Ctx, C1, C2, Size1, ShiftX1, ShiftY1) :-
    display_tiles('Replacement done - rebuild.',
        [0 > -1 - [C2, C1, C2, C2],
         1 > -1 - [C2, C2, C1, C1],
         0 > 0 - [C2, C2, C1, C2],
         1 > 0 - [C1, C1, C1, C2],
         0 > 1 - [C1, C1, C2, C2],
         1 > 1 - [C1, C2, C2, C1]
        ], Ctx, Size1, ShiftX1, ShiftY1),
    get_highlight_color(2, Color),
    display_edge_mark([0> -1 - 3, 0>0 - 1], Color, Ctx, Size1, ShiftX1, ShiftY1),
    ShiftX2 is ShiftX1 + 3,
    ShiftY2 is ShiftY1,
    Size2 is Size1 * 0.75,
    display_tiles('Hand tiles.',
        [0 > -1 - [C2, C1, C1, C2],
         0 > 0 - [C1, C2, C1, C2]
        ], Ctx, Size2, ShiftX2, ShiftY2, separator).

display_transform_4(Ctx, C1, C2, Size1, ShiftX1, ShiftY1) :-
    display_tiles('Transform done.',
        [-1 > -1 - [C2, C2, C1, C1],
         0 > -1 - [C2, C1, C2, C2],
         1 > -1 - [C2, C2, C1, C1],
         -1 > 0 - [C1, C2, C1, C2],
         0 > 0 - [C2, C2, C1, C2],
         1 > 0 - [C1, C1, C1, C2],
         0 > 1 - [C1, C1, C2, C2],
         1 > 1 - [C1, C2, C2, C1]
        ], Ctx, Size1, ShiftX1, ShiftY1),
    get_highlight_color(2, Color),
    display_edge_mark([0> -1 - 3, 0>0 - 1], Color, Ctx, Size1, ShiftX1, ShiftY1).

display_edge_mark([], _Color, _Ctx, _Size, _ShiftX, _ShiftY).
display_edge_mark([X>Y - Edge|T], Color, Ctx, Size, ShiftX, ShiftY) :-
    display_edge_mark1(X, Y, Edge, Color, Ctx, Size, ShiftX, ShiftY),
    display_edge_mark(T, Color, Ctx, Size, ShiftX, ShiftY).

display_tiles(Label, Defs, Ctx, Size, ShiftX, ShiftY) :-
    display_tiles(Label, Defs, Ctx, Size, ShiftX, ShiftY, no_separator).

display_tiles(Label, Defs, Ctx, Size, ShiftX, ShiftY, Separator) :-
    display_tiles(Defs, Ctx, Size, ShiftX, ShiftY),
    box(Defs, Left, Top, Right, Bottom),
    draw_label(Ctx, Label, ShiftX, ShiftY, Left, Top),
    (Separator = separator
      -> RightS is Right + ShiftX + 1,
         TopS is Top + ShiftY,
         BottomS is Bottom + ShiftY + 1,
         get_top_left_board_tile_coords(RightS, TopS, X, Y1),
         get_top_left_board_tile_coords(RightS, BottomS, X, Y2),
         XP is X + 20,
         Ctx >> [
            beginPath,
            moveTo(XP, Y1),
            lineTo(XP, Y2),
            closePath,
            save,
            strokeStyle <:+ '#000',
            stroke,
            restore
            ]
    ;
    true
    ).


display_tiles([], _Ctx, _Size, _ShiftX, _ShiftY).
display_tiles([H|T], Ctx, Size, ShiftX, ShiftY) :-
    display_tile(H, Ctx, Size, ShiftX, ShiftY),
    display_tiles(T, Ctx, Size, ShiftX, ShiftY).

display_tile(X>Y-selectable(Color), Ctx, Size, ShiftX, ShiftY) :-
    !,
    display_selectable_position(Ctx, Size, Color, ShiftX, ShiftY, X, Y).
display_tile(X>Y-nonselectable(Color), Ctx, Size, ShiftX, ShiftY) :-
    !,
    display_nonselectable_position(Ctx, Size, Color, ShiftX, ShiftY, X, Y).
display_tile(X>Y-Colors, Ctx, Size, ShiftX, ShiftY) :-
    draw_tile(Ctx, Size, Colors, ShiftX, ShiftY, X, Y).

draw_tile(Ctx, Size, Colors, BX, BY, GridX, GridY) :-
    GX is BX + GridX,
    GY is BY + GridY,
    get_top_left_board_tile_coords(GX, GY, X, Y),
    draw_tile(Ctx, Colors, Size, X, Y).

display_selectable_position(Ctx, Size, Color, ShiftX, ShiftY, GridX, GridY) :-
    GX is ShiftX + GridX,
    GY is ShiftY + GridY,
    get_top_left_board_tile_coords(GX, GY, X, Y),
    draw_legal_position(X, Y, Color, Ctx, Size).

display_nonselectable_position(Ctx, Size, Color, ShiftX, ShiftY, GridX, GridY) :-
    GX is ShiftX + GridX,
    GY is ShiftY + GridY,
    get_top_left_board_tile_coords(GX, GY, X, Y),
    draw_legal_position_with_rotation(X, Y, Color, Ctx, Size).

draw_label(Ctx, Msg, BX, BY, GridX, GridY) :-
    GX is BX + GridX,
    GY is BY + GridY,
    get_top_left_board_tile_coords(GX, GY, X, Y),
    Ctx >> [
        save,
        fillStyle <:+ '#000',
        fillText(Msg, X, Y-15),
        restore
    ].

box(Defs, Left, Top, Right, Bottom) :-
    box(Defs, 1000, 1000, Left, Top, -1000, -1000, Right, Bottom).

box([], Left, Top, Left, Top, Right, Bottom, Right, Bottom).
box([HX>HY - _|T], LeftIn, TopIn, LeftOut, TopOut, RightIn, BottomIn, RightOut, BottomOut) :-
    LeftNext is min(HX, LeftIn),
    TopNext is min(HY, TopIn),
    RightNext is max(HX, RightIn),
    BottomNext is max(HY, BottomIn),
    box(T, LeftNext, TopNext, LeftOut, TopOut, RightNext, BottomNext, RightOut, BottomOut).

% triangle from point offset by 1/3rd of center,
% to edge points offset by 1/3rd from corners.

display_edge_mark1(GridX, GridY, Edge, Color, Ctx, Size, ShiftX, ShiftY) :-
    GX is ShiftX + GridX,
    GY is ShiftY + GridY,
    get_top_left_board_tile_coords(GX, GY, X, Y),
    draw_edge_mark(X, Y, Edge, Color, Ctx, Size).

draw_edge_mark(X, Y, Edge, Color, Ctx, Size) :-
    MidX is X + (Size / 2),
    MidY is Y + (Size / 2),
    Adjust is Size / 6,
    edge_offsets(Edge, X, Y, Size, MidX, MidY, Adjust, P1, P2, Center),
    draw_triangle(P1, P2, Color, Center, Ctx).

edge_offsets(1, _X, Y, _Size, MX, MY, Adj, P1x>P1y, P2x>P2y, Cx > Cy) :-
    P1x is MX - Adj,
    P1y is Y,
    P2x is MX + Adj,
    P2y is Y,
    Cx is MX,
    Cy is MY - Adj.
edge_offsets(2, X, _Y, Size, MX, MY, Adj, P1x>P1y, P2x>P2y, Cx > Cy) :-
    P1x is X + Size,
    P1y is MY - Adj,
    P2x is X + Size,
    P2y is MY + Adj,
    Cx is MX + Adj,
    Cy is MY.
edge_offsets(3, _X, Y, Size, MX, MY, Adj, P1x>P1y, P2x>P2y, Cx > Cy) :-
    P1x is MX - Adj,
    P1y is Y + Size,
    P2x is MX + Adj,
    P2y is Y + Size,
    Cx is MX,
    Cy is MY + Adj.
edge_offsets(4, X, _Y, _Size, MX, MY, Adj, P1x>P1y, P2x>P2y, Cx > Cy) :-
    P1x is X,
    P1y is MY - Adj,
    P2x is X,
    P2y is MY + Adj,
    Cx is MX - Adj,
    Cy is MY.

display_replacement_tile_mark(GridX, GridY, Color, Ctx, Size, ShiftX, ShiftY) :-
    GX is ShiftX + GridX,
    GY is ShiftY + GridY,
    get_top_left_board_tile_coords(GX, GY, X, Y),
    draw_replacement_tile_mark(X, Y, Size, Color, Ctx).

/*
shape constraint 1: no holes

invalid:
- -
-
- -

valid:
-  -
----

*/

display_shape_1 :-
    _Canvas >> [id -:> shape_1, getContext('2d') *:> Ctx],
    get_player_color(1, C1),
    get_player_color(2, C2),
    get_board_tile_size(Size),
    display_shape_1_invalid(Ctx, C1, C2, Size, -10, -1),
    display_shape_1_valid(Ctx, C1, C2, Size, -7, -1).

display_shape_1_invalid(Ctx, C1, C2, Size, ShiftX, ShiftY) :-
    display_tiles('invalid - gap is 1 tile wide.',
         [0 > -1 - [C1, C1, C2, C2],
          1 > -1 - [C2, C1, C2, C1],
          0 > 0 - [C2, C2, C1, C2],
          0 > 1 - [C1, C2, C1, C2],
          1 > 1 - [C1, C2, C1, C2]
         ], Ctx, Size, ShiftX, ShiftY, separator).

display_shape_1_valid(Ctx, C1, C2, Size, ShiftX, ShiftY) :-
    display_tiles('valid - gap is 2 tiles wide.',
         [0 > -1 - [C1, C1, C2, C2],
          0 > 0 - [C2, C2, C1, C2],
          1 > 0 - [C1, C2, C1, C2],
          2 > 0 - [C2, C1, C1, C2],
          3 > 0 - [C1, C2, C1, C1],
          3 > -1 - [C1, C2, C1, C1]
         ], Ctx, Size, ShiftX, ShiftY).

/*
Six game displays showing first play, second play, third play with recency constraint and rotation.
Two canvases, A and B, three displays each.
*/

display_build_detail :-
    get_player_color(1, C1),
    get_player_color(2, C2),
    get_board_tile_size(Size),
    display_build_detail_a(C1, C2, Size),
    display_build_detail_b(C1, C2, Size).

display_build_detail_a(C1, C2, Size) :-
    _Canvas >> [id -:> build_detail_a, getContext('2d') *:> Ctx],
    display_build_detail_1(Ctx, C1, C2, Size, -10, -1),
    display_build_detail_2(Ctx, C1, C2, Size, -4, -1),
    display_build_detail_3(Ctx, C1, C2, Size, 2, -1).

display_build_detail_b(C1, C2, Size) :-
    _Canvas >> [id -:> build_detail_b, getContext('2d') *:> Ctx],
    display_build_detail_4(Ctx, C1, C2, Size, -10, -1),
    display_build_detail_5(Ctx, C1, C2, Size, -4, -1).

display_build_detail_1(Ctx, C1, C2, Size1, ShiftX1, ShiftY1) :-
    Size2 is Size1 * 0.75,
    display_tiles('Player 1.',
        [0 > -1 - [C1, C1, C1, C1],
         0 > 0 - [C2, C1, C1, C1],
         0 > 1 - [C1, C2, C2, C1]
        ],
        Ctx, Size2, ShiftX1, ShiftY1),
    get_highlight_color(1, Color),
    display_selected_tile_mark(0, -1, Color, Ctx, Size2, ShiftX1, ShiftY1),
    ShiftX2 is ShiftX1 + 2,
    ShiftY2 is ShiftY1,
    display_tiles('Board.',
        [0 > 0 - selectable(Color)
        ], Ctx, Size1, ShiftX2, ShiftY2),
    ShiftX3 is ShiftX2 + 2,
    ShiftY3 is ShiftY2,
    display_tiles('Player 2.',
        [0 > -1 - [C2, C2, C2, C2],
         0 > 0 - [C1, C2, C2, C2],
         0 > 1 - [C1, C2, C2, C1]
        ],
        Ctx, Size2, ShiftX3, ShiftY3, separator).

display_build_detail_2(Ctx, C1, C2, Size1, ShiftX1, ShiftY1) :-
    Size2 is Size1 * 0.75,
    display_tiles('Player 1.',
        [0 > 0 - [C2, C1, C1, C1],
         0 > 1 - [C1, C2, C2, C1]
        ],
        Ctx, Size2, ShiftX1, ShiftY1),
    get_highlight_color(2, Color),
    ShiftX2 is ShiftX1 + 2,
    ShiftY2 is ShiftY1,
    display_tiles('Board.',
        [0 > -1 - nonselectable(Color),
         -1 > 0 - nonselectable(Color),
         0 > 0 - [C1, C1, C1, C1],
         1 > 0 - nonselectable(Color),
         0 > 1 - selectable(Color)
        ], Ctx, Size1, ShiftX2, ShiftY2),
    ShiftX3 is ShiftX2 + 2,
    ShiftY3 is ShiftY2,
    display_tiles('Player 2.',
        [0 > -1 - [C2, C2, C2, C2],
         0 > 0 - [C1, C2, C2, C2],
         0 > 1 - [C1, C2, C2, C1]
        ],
        Ctx, Size2, ShiftX3, ShiftY3, separator),
    display_selected_tile_mark(0, 0, Color, Ctx, Size2, ShiftX3, ShiftY3).

display_build_detail_3(Ctx, C1, C2, Size1, ShiftX1, ShiftY1) :-
    Size2 is Size1 * 0.75,
    display_tiles('Player 1.',
        [0 > 0 - [C2, C1, C1, C1],
         0 > 1 - [C1, C2, C2, C1]
        ],
        Ctx, Size2, ShiftX1, ShiftY1),
    get_highlight_color(1, Color),
    display_selected_tile_mark(0, 0, Color, Ctx, Size2, ShiftX1, ShiftY1),
    ShiftX2 is ShiftX1 + 2,
    ShiftY2 is ShiftY1,
    display_tiles('Board.',
        [0 > 0 - [C1, C1, C1, C1],
         -1 > 1 - nonselectable(Color),
         0 > 1 - [C1, C2, C2, C2],
         1 > 1 - nonselectable(Color)
        ], Ctx, Size1, ShiftX2, ShiftY2),
    ShiftX3 is ShiftX2 + 2,
    ShiftY3 is ShiftY2,
    display_tiles('Player 2.',
        [0 > -1 - [C2, C2, C2, C2],
         0 > 1 - [C1, C2, C2, C1]
        ],
        Ctx, Size2, ShiftX3, ShiftY3).

display_build_detail_4(Ctx, C1, C2, Size1, ShiftX1, ShiftY1) :-
    Size2 is Size1 * 0.75,
    display_tiles('Player 1.',
        [0 > 0 - [C1, C2, C1, C1],
         0 > 1 - [C1, C2, C2, C1]
        ],
        Ctx, Size2, ShiftX1, ShiftY1),
    get_highlight_color(1, Color),
    display_selected_tile_mark(0, 0, Color, Ctx, Size2, ShiftX1, ShiftY1),
    ShiftX2 is ShiftX1 + 2,
    ShiftY2 is ShiftY1,
    display_tiles('Board.',
        [0 > 0 - [C1, C1, C1, C1],
         -1 > 1 - selectable(Color),
         0 > 1 - [C1, C2, C2, C2],
         1 > 1 - nonselectable(Color)
        ], Ctx, Size1, ShiftX2, ShiftY2),
    ShiftX3 is ShiftX2 + 2,
    ShiftY3 is ShiftY2,
    display_tiles('Player 2.',
        [0 > -1 - [C2, C2, C2, C2],
         0 > 1 - [C1, C2, C2, C1]
        ],
        Ctx, Size2, ShiftX3, ShiftY3, separator).

display_build_detail_5(Ctx, C1, C2, Size1, ShiftX1, ShiftY1) :-
    Size2 is Size1 * 0.75,
    display_tiles('Player 1.',
        [0 > 1 - [C1, C2, C2, C1]
        ],
        Ctx, Size2, ShiftX1, ShiftY1),
    ShiftX2 is ShiftX1 + 2,
    ShiftY2 is ShiftY1,
    display_tiles('Board.',
        [0 > 0 - [C1, C1, C1, C1],
         -1 > 1 - [C1, C2, C1, C1],
         0 > 1 - [C1, C2, C2, C2]
        ], Ctx, Size1, ShiftX2, ShiftY2),
    ShiftX3 is ShiftX2 + 2,
    ShiftY3 is ShiftY2,
    display_tiles('Player 2.',
        [0 > -1 - [C2, C2, C2, C2],
         0 > 1 - [C1, C2, C2, C1]
        ],
        Ctx, Size2, ShiftX3, ShiftY3).

display_selected_tile_mark(GridX, GridY, Color, Ctx, Size, ShiftX, ShiftY) :-
    GX is ShiftX + GridX,
    GY is ShiftY + GridY,
    get_top_left_board_tile_coords(GX, GY, X, Y),
    draw_selected_tile_mark(X, Y, Size, Color, Ctx).


display_transform_detail :-
    get_player_color(1, C1),
    get_player_color(2, C2),
    get_board_tile_size(Size),
    display_transform_detail_a(C1, C2, Size),
    display_transform_detail_b(C1, C2, Size),
    display_transform_detail_c(C1, C2, Size).

display_transform_detail_a(C1, C2, Size) :-
    _Canvas >> [id -:> transform_detail_a, getContext('2d') *:> Ctx],
    display_transform_detail_1(Ctx, C1, C2, Size, -10, -1),
    display_transform_detail_2(Ctx, C1, C2, Size, -6, -1),
    display_transform_detail_3(Ctx, C1, C2, Size, 0, -1).

display_transform_detail_b(C1, C2, Size) :-
    _Canvas >> [id -:> transform_detail_b, getContext('2d') *:> Ctx],
    display_transform_detail_4(Ctx, C1, C2, Size, -10, -1),
    display_transform_detail_5(Ctx, C1, C2, Size, -4, -1),
    display_transform_detail_6(Ctx, C1, C2, Size, 4, -1).

display_transform_detail_c(C1, C2, Size) :-
    _Canvas >> [id -:> transform_detail_c, getContext('2d') *:> Ctx],
    display_transform_detail_7(Ctx, C1, C2, Size, -10, -1),
    display_transform_detail_8(Ctx, C1, C2, Size, -3, -1).

display_transform_detail_1(Ctx, C1, C2, Size1, ShiftX1, ShiftY1) :-
    display_tiles('Select edge.',
        [-1 > -1 - [C2, C2, C1, C1],
         0 > -1 - [C2, C1, C2, C2],
         1 > -1 - [C2, C2, C1, C1],
         2 > -1 - [C1, C2, C1, C2],
         -1 > 0 - [C1, C2, C1, C2],
         0 > 0 - [C2, C2, C1, C2],
         1 > 0 - [C1, C1, C1, C2],
         0 > 1 - [C1, C1, C2, C2],
         1 > 1 - [C1, C2, C2, C1],
         1 > 2 - [C2, C2, C2, C2]
        ],
        Ctx, Size1, ShiftX1, ShiftY1, separator),
    get_highlight_color(1, Color),
    display_edge_mark([-1 > 0 - 2, 0>0 - 4], Color, Ctx, Size1, ShiftX1, ShiftY1).

display_transform_detail_2(Ctx, C1, C2, Size1, ShiftX1, ShiftY1) :-
    Size2 is Size1 * 0.75,
    display_tiles('Player 1.',
        [0 > -1 - [C1, C2, C1, C2],
         0 > 0 - [C2, C2, C1, C2]
        ],
        Ctx, Size2, ShiftX1, ShiftY1),
    ShiftX2 is ShiftX1 + 2,
    ShiftY2 is ShiftY1,
    get_highlight_color(1, Color),
    display_tiles('Board.',
        [-1 > -1 - [C2, C2, C1, C1],
         0 > -1 - [C2, C1, C2, C2],
         1 > -1 - [C2, C2, C1, C1],
         2 > -1 - [C1, C2, C1, C2],
         0 > 0 - selectable(Color),
         1 > 0 - [C1, C1, C1, C2],
         0 > 1 - [C1, C1, C2, C2],
         1 > 1 - [C1, C2, C2, C1],
         1 > 2 - [C2, C2, C2, C2]
        ],
        Ctx, Size1, ShiftX2, ShiftY2, separator),
    display_replacement_tile_mark(1, -1, Color, Ctx, Size1, ShiftX2, ShiftY2),
    display_replacement_tile_mark(1, 0, Color, Ctx, Size1, ShiftX2, ShiftY2),
    display_selected_tile_mark(1, -1, Color, Ctx, Size1, ShiftX2, ShiftY2).

display_transform_detail_3(Ctx, C1, C2, Size1, ShiftX1, ShiftY1) :-
    Size2 is Size1 * 0.75,
    display_tiles('Player 1.',
        [0 > -1 - [C1, C2, C1, C2],
         0 > 0 - [C2, C2, C1, C2]
        ],
        Ctx, Size2, ShiftX1, ShiftY1),
    ShiftX2 is ShiftX1 + 2,
    ShiftY2 is ShiftY1,
    get_highlight_color(1, Color),
    display_tiles('Board.',
        [-1 > -1 - [C2, C2, C1, C1],
         0 > -1 - [C2, C1, C2, C2],
         2 > -1 - [C1, C2, C1, C2],
         -1 > 0 - selectable(Color),
         0 > 0 - [C2, C2, C1, C1],
         1 > 0 - [C1, C1, C1, C2],
         0 > 1 - [C1, C1, C2, C2],
         1 > 1 - [C1, C2, C2, C1],
         1 > 2 - [C2, C2, C2, C2]
        ],
        Ctx, Size1, ShiftX2, ShiftY2),
    display_replacement_tile_mark(1, 0, Color, Ctx, Size1, ShiftX2, ShiftY2),
    display_selected_tile_mark(1, 0, Color, Ctx, Size1, ShiftX2, ShiftY2).

display_transform_detail_4(Ctx, C1, C2, Size1, ShiftX1, ShiftY1) :-
    Size2 is Size1 * 0.75,
    display_tiles('Player 1.',
        [0 > -1 - [C1, C2, C1, C2],
         0 > 0 - [C2, C2, C1, C2]
        ],
        Ctx, Size2, ShiftX1, ShiftY1),
    get_highlight_color(1, Color),
    display_replacement_tile_mark(0, 0, Color, Ctx, Size2, ShiftX1, ShiftY1),
    display_selected_tile_mark(0, 0, Color, Ctx, Size2, ShiftX1, ShiftY1),
    ShiftX2 is ShiftX1 + 2,
    ShiftY2 is ShiftY1,
    display_tiles('Board.',
        [-1 > -1 - [C2, C2, C1, C1],
         0 > -1 - [C2, C1, C2, C2],
         1 > -1 - selectable(Color),
         2 > -1 - [C1, C2, C1, C2],
         -1 > 0 - [C1, C1, C1, C2],
         0 > 0 - [C2, C2, C1, C1],
         0 > 1 - [C1, C1, C2, C2],
         1 > 1 - [C1, C2, C2, C1],
         1 > 2 - [C2, C2, C2, C2]
        ],
        Ctx, Size1, ShiftX2, ShiftY2, separator).

display_transform_detail_5(Ctx, C1, C2, Size1, ShiftX1, ShiftY1) :-
    Size2 is Size1 * 0.75,
    display_tiles('Player 1.',
        [0 > -1 - [C1, C2, C1, C2]
        ],
        Ctx, Size2, ShiftX1, ShiftY1),
    get_highlight_color(1, Color),
    display_replacement_tile_mark(0, -1, Color, Ctx, Size2, ShiftX1, ShiftY1),
    display_selected_tile_mark(0, -1, Color, Ctx, Size2, ShiftX1, ShiftY1),
    ShiftX2 is ShiftX1 + 2,
    ShiftY2 is ShiftY1,
    display_tiles('Board.',
        [-1 > -1 - [C2, C2, C1, C1],
         0 > -1 - [C2, C1, C2, C2],
         1 > -1 - [C2, C2, C1, C2],
         2 > -1 - [C1, C2, C1, C2],
         -1 > 0 - [C1, C1, C1, C2],
         0 > 0 - [C2, C2, C1, C1],
         1 > 0 - selectable(Color),
         0 > 1 - [C1, C1, C2, C2],
         1 > 1 - [C1, C2, C2, C1],
         1 > 2 - [C2, C2, C2, C2]
        ],
        Ctx, Size1, ShiftX2, ShiftY2, separator),
    get_highlight_color(2, OtherColor),
    display_edge_mark1(0, -1, 2, OtherColor, Ctx, Size1, ShiftX2, ShiftY2).

display_transform_detail_6(Ctx, C1, C2, Size1, ShiftX1, ShiftY1) :-
    Size2 is Size1 * 0.75,
    get_highlight_color(2, Color),
    display_tiles('Board.',
        [-1 > -1 - [C2, C2, C1, C1],
         0 > -1 - selectable(Color),
         1 > -1 - [C2, C2, C1, C2],
         2 > -1 - [C1, C2, C1, C2],
         -1 > 0 - [C1, C1, C1, C2],
         0 > 0 - [C2, C2, C1, C1],
         1 > 0 - [C1, C2, C1, C2],
         0 > 1 - [C1, C1, C2, C2],
         1 > 1 - [C1, C2, C2, C1],
         1 > 2 - [C2, C2, C2, C2]
        ],
        Ctx, Size1, ShiftX1, ShiftY1),
    display_replacement_tile_mark(1, 2, Color, Ctx, Size1, ShiftX1, ShiftY1),
    display_selected_tile_mark(1, 2, Color, Ctx, Size1, ShiftX1, ShiftY1),
    ShiftX2 is ShiftX1 + 4,
    ShiftY2 is ShiftY1,
    display_tiles('Player 2.',
        [0 > -1 - [C2, C1, C2, C2]
        ],
        Ctx, Size2, ShiftX2, ShiftY2).

display_transform_detail_7(Ctx, C1, C2, Size1, ShiftX1, ShiftY1) :-
    Size2 is Size1 * 0.75,
    get_highlight_color(2, Color),
    display_tiles('Board.',
        [-1 > -2 - selectable(Color),
         0 > -2 - selectable(Color),
         1 > -2 - selectable(Color),
         2 > -2 - nonselectable(Color),
         -2 > -1 - selectable(Color),
         -1 > -1 - [C2, C2, C1, C1],
         0 > -1 - [C2, C2, C2, C2],
         1 > -1 - [C2, C2, C1, C2],
         2 > -1 - [C1, C2, C1, C2],
         -2 > 0 - nonselectable(Color),
         -1 > 0 - [C1, C1, C1, C2],
         0 > 0 - [C2, C2, C1, C1],
         1 > 0 - [C1, C2, C1, C2],
         2 > 0 - nonselectable(Color),
         -1 > 1 - nonselectable(Color),
         0 > 1 - [C1, C1, C2, C2],
         1 > 1 - [C1, C2, C2, C1]
        ],
        Ctx, Size1, ShiftX1, ShiftY1),
    ShiftX2 is ShiftX1 + 4,
    ShiftY2 is ShiftY1,
    display_tiles('Player 2.',
        [0 > -1 - [C2, C1, C2, C2]
        ],
        Ctx, Size2, ShiftX2, ShiftY2, separator),
    display_selected_tile_mark(0, -1, Color, Ctx, Size2, ShiftX2, ShiftY2).

display_transform_detail_8(Ctx, C1, C2, Size1, ShiftX1, ShiftY1) :-
    display_tiles('Board.',
        [0 > -2 - [C2, C1, C2, C2],
         -1 > -1 - [C2, C2, C1, C1],
         0 > -1 - [C2, C2, C2, C2],
         1 > -1 - [C2, C2, C1, C2],
         2 > -1 - [C1, C2, C1, C2],
         -1 > 0 - [C1, C1, C1, C2],
         0 > 0 - [C2, C2, C1, C1],
         1 > 0 - [C1, C2, C1, C2],
         0 > 1 - [C1, C1, C2, C2],
         1 > 1 - [C1, C2, C2, C1]
        ],
        Ctx, Size1, ShiftX1, ShiftY1).
