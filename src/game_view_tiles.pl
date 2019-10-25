:- module(game_view_tiles, [create_game_view_tiles/0, layout_hands/0, center_board/0,
    get_translate_x/1, set_translate_x/1, get_translate_y/1, set_translate_y/1,
    get_target_translate_x/1, set_target_translate_x/1, get_target_translate_y/1, set_target_translate_y/1,
    get_top_left_board_tile_coords/4, point_in_board_position/4, update_board_tile_view/1,
    game_view_tiles_values/1, game_view_tiles_values/2]).

:- use_module(library).
:- use_module('../proscriptls_sdk/library/data_predicates').
:- use_module(tile_model).
:- use_module(game_model_tiles).
:- use_module(tile_view).
:- use_module(view_basics).
:- use_module(geometry).

:- initialization(initdyn).

initdyn :-
    data_predicate_dynamics([data_predicates(gvt, data,[translateX, translateY, targetTranslateX, targetTranslateY])]).

create_game_view_tiles :-
    assert_data(gvt(0,0,0,0), 1).

layout_hands :-
    get_hands(Hands),
    %forall(nth1(HandID, Hands, Hand), layout_hand(HandID, Hand)).
    layout_hands(Hands, 1).

layout_hands([], _).
layout_hands([H|T], ID) :-
    layout_hand(ID, H),
    NextID is ID + 1,
    layout_hands(T, NextID).

layout_hand(HandID, HandTiles) :-
    get_hand_padding(Padding),
    get_hand_margin(Margin),
    get_hand_tile_size(TileSize),
    (HandID = 1
      -> X = Margin
    ;
     get_canvas_width(Width),
     X is Width - (Margin + TileSize)
    ),
    layout_hand(HandTiles, 0, X, Margin, TileSize, Padding).

% layout_hand(HandTiles, Offset, TileSize, Padding)
layout_hand([], _, _, _, _, _).
layout_hand([H|T], Offset, X, Y, TileSize, Padding) :-
    layout_hand_tile(H, Offset, X, Y, TileSize, Padding),
    NextOffset is Offset + 1,
    layout_hand(T, NextOffset, X, Y, TileSize, Padding).

layout_hand_tile(Tile, Offset, X, Y, TileSize, Padding) :-
    set_tile_size(Tile, TileSize),
    set_tile_display_x(Tile, X),
    AdjustedY is Y + Offset * (TileSize + Padding),
    set_tile_display_y(Tile, AdjustedY).

center_board :-
    get_board(Board),
    length(Board, Length),
    (Length = 0
      -> set_translate_x(0),
         set_translate_y(0)
    ;
    get_canvas_width(Left),
    Right = 0,
    get_canvas_height(Top),
    Bottom = 0,
    get_board(BoardTiles),
    adjust_tiles(BoardTiles, x(Top, Left, Bottom, Right), x(AdjustTop, AdjustLeft, AdjustBottom, AdjustRight)),
    data_translateX(TranslateX),
    data_translateY(TranslateY),
    get_board_left(BoardLeft),
    get_board_width(BoardWidth),
    get_board_top(BoardTop),
    get_board_height(BoardHeight),
    TargetTranslateX is TranslateX - ((AdjustRight + AdjustLeft) / 2 + (BoardLeft + BoardWidth) / 2),
    TargetTranslateY is TranslateY - ((AdjustBottom + AdjustTop) / 2 + (BoardTop + BoardHeight) / 2),
    set_target_translate_x(TargetTranslateX),
    set_target_translate_y(TargetTranslateY)
    ).


adjust_tiles([], Adjust, Adjust).
adjust_tiles([Tile|OtherTiles], In, Out) :-
    adjust_tile(Tile, In, Next),
    adjust_tiles(OtherTiles, Next, Out).

adjust_tile(Tile, x(Top, Left, Bottom, Right), x(AdjustTop, AdjustLeft, AdjustBottom, AdjustRight)) :-
    get_tile_display_x(Tile, X),
    get_tile_display_y(Tile, Y),
    get_tile_size(Tile, Size),
    AdjustTop is min(Top, Y),
    AdjustLeft is min(Left, X),
    AdjustBottom is max(Bottom, Y + Size),
    AdjustRight is max(Right, X + Size).

get_translate_x(Value) :-
    data_translateX(Value).

set_translate_x(Value) :-
    data_default_id(ID),
    retract(data_translateX(ID, _)),
    asserta(data_translateX(ID, Value)).

get_translate_y(Value) :-
    data_translateY(Value).

set_translate_y(Value) :-
    data_default_id(ID),
    retract(data_translateY(ID, _)),
    asserta(data_translateY(ID, Value)).

get_target_translate_x(Value) :-
    data_targetTranslateX(Value).

set_target_translate_x(Value) :-
    data_default_id(ID),
    retract(data_targetTranslateX(ID, _)),
    asserta(data_targetTranslateX(ID, Value)).

get_target_translate_y(Value) :-
    data_targetTranslateY(Value).

set_target_translate_y(Value) :-
    data_default_id(ID),
    retract(data_targetTranslateY(ID, _)),
    asserta(data_targetTranslateY(ID, Value)).

get_top_left_board_tile_coords(GridX, GridY, X, Y) :-
    get_board_tile_size(TileSize),
    get_board_left(BoardLeft),
    get_board_width(BoardWidth),
    get_board_top(BoardTop),
    get_board_height(BoardHeight),
    data_translateX(TranslateX),
    data_translateY(TranslateY),
    X is BoardLeft + TranslateX + BoardWidth / 2 + (GridX - 0.5) * TileSize,
    Y is BoardTop + TranslateY + BoardHeight / 2 + (GridY - 0.5) * TileSize.

point_in_board_position(GridX, GridY, X, Y) :-
    get_top_left_board_tile_coords(GridX, GridY, TX, TY),
    get_board_tile_size(TileSize),
    in_square(X, Y, TX, TY, TileSize).

update_board_tile_view(Tile) :-
    get_tile_grid_x(Tile, GridX),
    get_tile_grid_y(Tile, GridY),
    get_board_tile_size(Size),
    set_tile_size(Tile, Size),
    get_top_left_board_tile_coords(GridX, GridY, ViewX, ViewY),
    set_tile_display_x(Tile, ViewX),
    set_tile_display_y(Tile, ViewY).

game_view_tiles_values(Values) :-
    data_default_id(ID),
    game_view_tiles_values(ID, Values).

game_view_tiles_values(ID, Values) :-
    labelled_values(data, ID, Values).