:- module(view_basics,
    [create_view_basics/0, view_basics_values/1,
     get_canvas_width/1, get_canvas_height/1, get_canvas_offset_top/1, get_canvas_offset_left/1,
     get_context/1, get_player_color/2, get_highlight_color/2,
     get_hand_tile_size/1, get_hand_padding/1, get_hand_margin/1, get_board_tile_size/1,
     get_board_left/1, get_board_top/1, get_board_width/1, get_board_height/1
]).

:- use_module('../proscriptls_sdk/library/data_predicates').
:- use_module(library).
:- use_module('../proscriptls_sdk/library/object').

:- initialization(initdyn).

initdyn :-
    data_predicate_dynamics([data_predicates(vb, data,[canvasWidth, canvasHeight, canvasOffsetTop, canvasOffsetLeft, context,
        colors, highlightColors, handTileSize, handPadding, handMargin,
        boardTileSize, boardLeft, boardTop, boardWidth, boardHeight])]).

create_view_basics :-
        _Canvas >> [id -:> canvas,
            getContext('2d') *:> Ctx,
            width +:> W,
            height +:> H,
            @ dom_page_offset(OffsetTop, OffsetLeft)],
        assert_data(
            vb(W, H, OffsetTop, OffsetLeft, Ctx,
               ['#008800', '#4444FF'], ['#CCFFCC', '#CCCCFF'],
               55, 4, 5, 75, 100, 0, 800, 600), 1).

get_canvas_width(X) :- data_canvasWidth(X).

get_canvas_height(X) :- data_canvasHeight(X).

get_canvas_offset_top(X) :- data_canvasOffsetTop(X).

get_canvas_offset_left(X) :- data_canvasOffsetLeft(X).

get_context(X) :- data_context(X).

get_player_color(ID, X) :- data_colors(Colors), nth1(ID, Colors,  X).

get_highlight_color(ID, X) :- data_highlightColors(Colors), nth1(ID, Colors,  X).

get_hand_tile_size(X) :- data_handTileSize(X).

get_hand_padding(X) :- data_handPadding(X).

get_hand_margin(X) :- data_handMargin(X).

get_board_tile_size(X) :- data_boardTileSize(X).

get_board_left(X) :- data_boardLeft(X).

get_board_top(X) :- data_boardTop(X).

get_board_width(X) :- data_boardWidth(X).

get_board_height(X) :- data_boardHeight(X).

view_basics_values(Values) :-
    data_default_id(ID),
    labelled_values(data, ID, Values).