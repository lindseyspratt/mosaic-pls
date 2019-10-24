:- module(tile_view,
    [create_tile_view/4,
     get_id/2, get_tile_display_x/2, set_tile_display_x/2, get_tile_display_y/2, set_tile_display_y/2, get_tile_size/2, set_tile_size/2,
     point_in_tile/3, point_in_tile_edge/4, tile_view_values/2]).

:- use_module('../proscriptls_sdk/library/data_predicates').

:- initialization(initdyn).

initdyn :-
    data_predicate_dynamics([data_predicates(tv, data,[displayX, displayY, size])]).

create_tile_view(ID, DisplayX, DisplayY, Size) :-
    assert_data(tv(DisplayX, DisplayY, Size), ID).

get_tile_display_x(ID, Value) :-
    data_displayX(ID, Value).

set_tile_display_x(ID, Value) :-
    retractall(data_displayX(ID, _)),
    asserta(data_displayX(ID, Value)).

get_tile_display_y(ID, Value) :-
    data_displayY(ID, Value).

set_tile_display_y(ID, Value) :-
    retractall(data_displayY(ID, _)),
    asserta(data_displayY(ID, Value)).

get_tile_size(ID, Value) :-
    data_size(ID, Value).

set_tile_size(ID, Value) :-
    retractall(data_size(ID, _)),
    asserta(data_size(ID, Value)).

point_in_tile(ID, X, Y) :-
    data_displayX(ID, DX),
    data_displayY(ID, DY),
    data_size(ID, Size),
    X >= DX,
    X =< (DX + Size),
    Y >= DY,
    Y =< DY + Size.

point_in_tile_edge(ID, X, Y, Edge) :-
    data_displayX(ID, DX),
    data_displayY(ID, DY),
    data_size(ID, Size),
    (\+ point_in_tile(ID, X, Y)
        -> throw(click_point_not_in_tile(X, Y, ID))
    ;
     TopD is Y - DY,
     RightD is DX + Size - X,
     BottomD is DY + Size - Y,
     LeftD is X - DX,
     (TopD =< RightD
       -> (TopD =< BottomD
             -> (TopD =< LeftD
                  -> Edge = 0 % top
                ;
                 Edge = 3 % left
                )
          ;
           BottomD =< LeftD
             -> Edge = 2 % bottom
          ;
           Edge = 3 % left
          )
     ;
      RightD =< BottomD
       -> (RightD =< LeftD
            -> Edge = 1 % right
          ;
           Edge = 3 % left
          )
     ;
      BottomD =< LeftD
       -> Edge = 2 % bottom
     ;
      Edge = 3 % left
     )
    ).

tile_view_values(ID, Values) :-
    labelled_values(data, ID, Values).