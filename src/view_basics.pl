:- module(view_basics,
    [create_view_basics/0, save_view_basics/0, load_view_basics/0,
     save_view_basics_stream/1, retract_view_basics/0, reset_view_basics/0,
     view_basics_values/1,
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
    data_predicate_dynamics(
        [data_predicates(vb, data, [undoable],
            [canvasWidth, canvasHeight, canvasOffsetTop, canvasOffsetLeft, context,
             colors, highlightColors, handTileSize, handPadding, handMargin,
             boardTileSize, boardLeft, boardTop, boardWidth, boardHeight
            ])
        ]).

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

reset_view_basics :-
          _Canvas >> [id -:> canvas,
              getContext('2d') *:> Ctx,
              width +:> W,
              height +:> H,
              @ dom_page_offset(OffsetTop, OffsetLeft)],
          set_context(Ctx),
          set_canvas_height(H),
          set_canvas_width(W),
          set_canvas_offset_top(OffsetTop),
          set_canvas_offset_left(OffsetLeft).

save_view_basics_stream(Stream) :-
    save_data_stream(data, Stream).

retract_view_basics :-
    retract_all_data(data).

save_view_basics :-
    save_data(data, local_storage('mosaic')).

% Load the saved data and reset the transient
% elements that depend on the current canvas:
% height, width, offset_top, offset_left, and
% 2d context.

load_view_basics :-
    load_data(data, local_storage('mosaic')),
    _Canvas >> [id -:> canvas,
        getContext('2d') *:> Ctx,
        width +:> W,
        height +:> H,
        @ dom_page_offset(OffsetTop, OffsetLeft)],
    set_canvas_height(H),
    set_canvas_width(W),
    set_canvas_offset_top(OffsetTop),
    set_canvas_offset_left(OffsetLeft),
    set_context(Ctx).

get_canvas_width(X) :- data_canvasWidth(X).

set_canvas_width(X) :-
    data_default_id(ID),
    retractall(data_canvasWidth(ID, _)),
    asserta(data_canvasWidth(ID, X)).

get_canvas_height(X) :- data_canvasHeight(X).

set_canvas_height(X) :-
    data_default_id(ID),
    retractall(data_canvasHeight(ID, _)),
    asserta(data_canvasHeight(ID, X)).

get_canvas_offset_top(X) :- data_canvasOffsetTop(X).

set_canvas_offset_top(X) :-
    data_default_id(ID),
    retractall(data_canvasOffsetTop(ID, _)),
    asserta(data_canvasOffsetTop(ID, X)).

get_canvas_offset_left(X) :- data_canvasOffsetLeft(X).

set_canvas_offset_left(X) :-
    data_default_id(ID),
    retractall(data_canvasOffsetLeft(ID, _)),
    asserta(data_canvasOffsetLeft(ID, X)).

get_context(X) :- data_context(X).

set_context(X) :-
    data_default_id(ID),
    retractall(data_context(ID, _)),
    asserta(data_context(ID, X)).

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