:- module(game_view_tiles,
    [create_game_view_tiles/0, save_game_view_tiles/0, load_game_view_tiles/0,
     save_game_view_tiles_stream/1, retract_game_view_tiles/0,
    layout_hands/0, center_board/0,
    get_translate_x/1, set_translate_x/1, get_translate_y/1, set_translate_y/1,
    get_target_translate_x/1, set_target_translate_x/1, get_target_translate_y/1, set_target_translate_y/1,
    get_top_left_board_tile_coords/4, point_in_board_position/4, update_board_tile_view/1, reposition_board_toward_target_translation/0,
    game_view_tiles_values/1, game_view_tiles_values/2]).

:- use_module(library).
:- use_module('../proscriptls_sdk/library/data_predicates').
:- use_module(model_basics).
:- use_module(tile_model).
:- use_module(game_model_tiles).
:- use_module(tile_view).
:- use_module(view_basics).
:- use_module(geometry).

:- initialization(initdyn).

initdyn :-
    data_predicate_dynamics([data_predicates(gvt, data, [ephemeral], [translateX, translateY, targetTranslateX, targetTranslateY])]).

create_game_view_tiles :-
    assert_data(gvt(0,0,0,0), 1).

save_game_view_tiles_stream(Stream) :-
    save_data_stream(data, Stream).

retract_game_view_tiles :-
    retract_all_data(data).

save_game_view_tiles :-
    save_data(data, local_storage('mosaic')).

load_game_view_tiles :-
    load_data(data, local_storage('mosaic')).

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
    get_label_height(LabelHeight),
%    get_label_width(LabelWidth),
    get_hand_tile_size(TileSize),
    (HandID = 1
      -> X = Margin,
         Y is Margin + LabelHeight,
         Direction = y
    ;
     HandID = 2
      -> get_canvas_width(Width),
         get_number_of_players(NOP),
         X is Width - (NOP - 1) * (Margin + TileSize),
         Y is Margin + LabelHeight,
         Direction = y
    ;
     HandID = 3
      -> get_number_of_players(NOP),
         X is Margin + NOP * TileSize,
         Y is Margin + LabelHeight,
         Direction = x
    ;
     HandID = 4
      -> get_canvas_height(Height),
         get_number_of_players(NOP),
         X is Margin + NOP * TileSize,
         Y is Height - ((NOP - 1) * (Margin + TileSize) + LabelHeight),
         Direction = x
    ),
    layout_hand(HandTiles, 0, 0, 8, X, Y, Direction, TileSize, Padding).

% layout_hand(HandTiles, Offset, TileSize, Padding)
layout_hand([], _, _, _, _, _, _, _, _).
layout_hand([H|T], Offset, SegmentOffset, Segment, X, Y, Direction, TileSize, Padding) :-
    layout_hand_tile(H, Offset, SegmentOffset, X, Y, Direction, TileSize, Padding),
    (Segment > 1
      -> NextSegment is Segment - 1,
         NextSegmentOffset is SegmentOffset,
         NextOffset is Offset + 1
    ;
     NextSegment = 8,
     NextSegmentOffset is SegmentOffset + 1,
     NextOffset = 0
    ),
    layout_hand(T, NextOffset, NextSegmentOffset, NextSegment, X, Y, Direction, TileSize, Padding).

layout_hand_tile(Tile, Offset, SegmentOffset, X, Y, Direction, TileSize, Padding) :-
    set_tile_size(Tile, TileSize),
    (Direction = y
      -> AdjustedX is X + SegmentOffset * (TileSize + Padding),
         AdjustedY is Y + Offset * (TileSize + Padding),
         set_tile_display_x(Tile, AdjustedX),
         set_tile_display_y(Tile, AdjustedY)
    ;
     AdjustedY is Y + SegmentOffset * (TileSize + Padding),
     AdjustedX is X + Offset * (TileSize + Padding),
     set_tile_display_x(Tile, AdjustedX),
     set_tile_display_y(Tile, AdjustedY)
    ).

% fails if no repositioning to do.
reposition_board_toward_target_translation :-
    get_translate_x(TX),
    get_translate_y(TY),
    get_target_translate_x(TargetTX),
    get_target_translate_y(TargetTY),
    (TX =\= TargetTX; TY =\= TargetTY),
    repo(TX, TY, TargetTX, TargetTY).

repo(_TX, _TY, TargetTX, TargetTY) :-
%    NewTX is (TX * 3 + TargetTX) / 4,
%    NewTY is (TY * 3 + TargetTY) / 4,
%
%    (abs(NewTX - TargetTX) =< 2
%      -> FinalTX = TargetTX
%    ; FinalTX = NewTX
%    ),
%    (abs(TY - TargetTY) =< 2
%      -> FinalTY = TargetTY
%    ; FinalTY = NewTY
%    ),

    FinalTX = TargetTX,
    FinalTY = TargetTY,

    set_translate_x(FinalTX),
    set_translate_y(FinalTY),

    get_board(Board),
    repo1(Board).

repo1([]).
repo1([H|T]) :-
    repo2(H),
    repo1(T).

repo2(Tile) :-
    get_board_tile_grid(Tile, GridX > GridY),
    get_top_left_board_tile_coords(GridX, GridY, X, Y), % this predicate uses the translate (X/Y) values set in repo/4.
    set_tile_display_x(Tile, X),
    set_tile_display_y(Tile, Y).

/*
The 'display' positions of the tiles have the translate_x/y values included when calculating
from the grid x/y values.
Thus, the targetTranslate x/y values subtract the 'adjust' coordinates from the translate x/y values.
*/
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
    TargetTranslateX is (TranslateX - (AdjustRight + AdjustLeft) / 2) + (BoardLeft + BoardWidth) / 2,
    TargetTranslateY is (TranslateY - (AdjustBottom + AdjustTop) / 2) + (BoardTop + BoardHeight) / 2,
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
    set_data_translateX(ID, Value).

get_translate_y(Value) :-
    data_translateY(Value).

set_translate_y(Value) :-
    data_default_id(ID),
    set_data_translateY(ID, Value).

get_target_translate_x(Value) :-
    data_targetTranslateX(Value).

set_target_translate_x(Value) :-
    data_default_id(ID),
    set_data_targetTranslateX(ID, Value).

get_target_translate_y(Value) :-
    data_targetTranslateY(Value).

set_target_translate_y(Value) :-
    data_default_id(ID),
    set_data_targetTranslateY(ID, Value).

get_top_left_board_tile_coords(GridX, GridY, X, Y) :-
    get_board_tile_size(TileSize),
    get_board_left(BoardLeft),
    get_board_width(BoardWidth),
    get_board_top(BoardTop),
    get_board_height(BoardHeight),
    data_translateX(TranslateX),
    data_translateY(TranslateY),
    calculate_top_left_tile_coords(
        GridX, GridY, X, Y, TileSize,
        BoardTop, BoardLeft, BoardWidth, BoardHeight,
        TranslateX, TranslateY).

point_in_board_position(GridX, GridY, X, Y) :-
    get_top_left_board_tile_coords(GridX, GridY, TX, TY),
    get_board_tile_size(TileSize),
    in_square(X, Y, TX, TY, TileSize).

update_board_tile_view(Tile) :-
    get_board_tile_grid(Tile, GridX > GridY),
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
