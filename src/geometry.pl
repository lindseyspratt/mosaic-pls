:- module(geometry, [in_interval/3, in_square/5, calculate_top_left_tile_coords/11]).

in_square(X, Y, Left, Top, Size) :-
    in_interval(X, Left, Left+Size),
    in_interval(Y, Top, Top+Size).

in_interval(V, Low, High) :-
    V >= Low,
    V =< High.

calculate_top_left_tile_coords(
         GridX, GridY, X, Y, TileSize,
         BoardTop, BoardLeft, BoardWidth, BoardHeight,
         TranslateX, TranslateY) :-
    calculate_tile_coord(GridX, X, TileSize, BoardLeft, BoardWidth, TranslateX),
    calculate_tile_coord(GridY, Y, TileSize, BoardTop, BoardHeight, TranslateY).

calculate_tile_coord(GridCoord, DisplayCoord, TileSize, OriginCoord, CoordExtent, TranslateCoord) :-
    DisplayCoord is OriginCoord + TranslateCoord + CoordExtent / 2 + (GridCoord - 0.5) * TileSize.
