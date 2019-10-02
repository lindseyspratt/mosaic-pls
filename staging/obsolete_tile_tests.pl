
draw_tile_test :-
    _ >> [id -:> canvas, getContext('2d') *:> Ctx],
    assert_data(ts(20, 20, 1, 1, 50, [red, red, green, green], none), 1),
    draw_tile(Ctx, 1).

draw_all_tile_test :-
    _ >> [id -:> canvas, getContext('2d') *:> Ctx],
    assert_data(ts(20, 20, 1, 1, 50, [red, red, green, green], none), 1),
    draw_all_tile(1, Ctx).

draw_all_tiles_test :-
    setup_draw_all_tiles_test(Ctx, W, H, Tiles),
    draw_all_tiles(Tiles, Ctx, W, H).

setup_draw_all_tiles_test(Ctx, W, H, [Tile1, Tile2]) :-
    _ >> [id -:> canvas, getContext('2d') *:> Ctx, width +:> W, height +:> H],

    Tile1 = 1,
    Tile2 = 2,
    assert_data(ts(20, 20, 1, 1, 50, [red, red, green, green], none), 1),
    assert_data(ts(70, 20, 2, 1, 50, [green, red, green, red], none), 2),
% [tile_size, board_left, board_top, board_width, board_height, board_translate, turn, replacements]
    assert_data(g(50, 0, 0, 800, 800, 0, 1, [Tile2]), 1),

    retractall(is_selected(_)),
    asserta(is_selected(Tile1)).

draw_legal_moves_test :-
    setup_legal_moves(Ctx, LPs1, LPs2),
    draw_legal_moves(LPs1, LPs2, Ctx).

setup_legal_moves(Ctx, [1],[Tile2]) :-
    _ >> [id -:> canvas, getContext('2d') *:> Ctx],
    Tile2 = 2,
% [tile_size, board_left, board_top, board_width, board_height, board_translate, turn, replacements]
    assert_data(g(50, 10, 10, 200, 200, 1>1, 1, [Tile2]), 1),
    assert_data(lp(1,1), 1),
    assert_data(lp(1,2), 2).

display_hands_test :-
    _ >> [id -:> canvas, getContext('2d') *:> Ctx, width +:> W, height +:> H],
    assert_data(g(50, 10, 10, 800, 800, 1>1, 1, []), 1),
    initial_hands_expanded(2, Hands),
    setup_hands(Hands, TileIDs),
    draw_all_tiles(TileIDs, Ctx, W, H).
