:- module(letters, [display_letter/6, display_letters/6]).

:- use_module(draw).
:- use_module(geometry).
/*
display letters using tiles: M, O, S, A, I, C

*/
tile(sf1, [1,2,2,1]). % slash forward 1 left
tile(sf2, [2,1,1,2]). % slash forward 2 left
tile(sb1, [1,1,2,2]). % slash backward 1 right
tile(sb2, [2,2,1,1]). % slash backward 2 right
tile(c1u, [1,2,2,2]). % up 1 chevron
tile(c1l, [2,2,2,1]). % left 1 chevron
tile(c1r, [2,1,2,2]). % right 1 chevron
tile(c1d, [2,2,1,2]). % down 1 chevron
tile(c2u, [2,1,1,1]). % up 2 chevron
tile(c2l, [1,1,1,2]). % left 2 chevron
tile(c2r, [1,2,1,1]). % right 2 chevron
tile(c2d, [1,1,2,1]). % down 2 chevron
tile(solid1, [1,1,1,1]). % solid 1
tile(solid2, [2,2,2,2]). % solid 2

letter(m, [c2u-(0>0), sf1-(0>1), sf2-(1>1), sf1-(1 > 2), c1u-(2 > 2), c2u-(2 > 1),
    sb1-(3>2), sb2-(3>1), sb1-(4 > 1), c2u-(4>0)]).
letter(o, [c2r-(0>1), c1r-(1>1), sf1-(1 > 2), sb1-(2>2), c1l-(2>1), c2l-(3>1), sb2-(1>0), sf2-(2>0)]).
letter(s, [c2r-(0>0), sf2-(1>0), c1r-(1>1), sf1-(1>2), c2l-(2>2)]).
letter(a, [sf1-(0>0), c1d-(1>0),sf1-(1>1),c1u-(2>0),c1d-(2>1),c2d-(2>2),c1d-(3>0),sb1-(3>1), sb1-(4>0)]).
letter(i, [c1r-(0>0), solid2-(0>1), c1l-(0>2)]).
letter(c, [sb2-(0>0), c1r-(0>1), sf1-(0>2), c1u-(1>0), solid1-(1>1), c1d-(1>2)]).
letter(v, [solid1-(0>2), sb2-(0>1), sb1-(1>1), sf1-(2>1), sf2-(3>1), sb2-(1>0), sf2-(2>0)]).
letter('1', [solid2-(0>0), solid2-(0>1), solid2-(0>2)]).
letter('0', [solid2-(0>0), solid2-(0>1), solid2-(0>2), solid2-(1>0), solid1-(1>1), solid2-(1>2), solid2-(2>0), solid2-(2>1), solid2-(2>2)]).
letter(dot, [solid2-(0>0), solid1-(0>1)]).

display_letters([], _Ctx, _Size, _Top, Left, Left).
display_letters([H|T], Ctx, Size, Top, LeftIn, Left) :-
    display_letter(H, Ctx, Size, Top, LeftIn, MaxX),
    NextLeft is 10 + LeftIn + (MaxX+1) * Size,
    display_letters(T, Ctx, Size, Top, NextLeft, Left).

% letter coords are from (0,0) (lower left corner) up to (+X, +Y) upper right corner.
% The display coords have the Y axis inverted, with 0 above 1, so the letter coords
% must have their Y values inverted.

display_letter(Letter, Ctx, Size, Top, Left, MaxX) :-
    letter(Letter, TileCoords),
    display_letter_coords(TileCoords, Ctx, Size, Top, Left, MaxX).

display_letter_coords(TileCoords, Ctx, Size, Top, Left, MaxX) :-
    process_tiles(TileCoords, MaxX, MaxY),
    display_tiles(TileCoords, info(Ctx, Size, Top, Left, MaxY)).

process_tiles(TileCoords, MaxX, MaxY) :-
    process_tiles(TileCoords, 0, MaxX, 0, MaxY).

process_tiles([], MaxX, MaxX, MaxY, MaxY).
process_tiles([_-(X>Y)|T], MaxXIn, MaxX, MaxYIn, MaxY) :-
    (X > MaxXIn -> MaxXNext = X;MaxXIn = MaxXNext),
    (Y > MaxYIn -> MaxYNext = Y;MaxYIn = MaxYNext),
    process_tiles(T, MaxXNext, MaxX, MaxYNext, MaxY).

display_tiles([], _).
display_tiles([H|T], Info) :-
    display_tile(H, Info),
    display_tiles(T, Info).

display_tile(Tile-(GridX > InverseGridY), info(Ctx, Size, Top, Left, MaxY)) :-
    GridY is MaxY - InverseGridY,
    calculate_top_left_tile_coords(
            GridX, GridY, X, Y, Size,
            Top, Left, 0, 0,
            0, 0),
    tile(Tile, AbstractColors),
    colors_map(AbstractColors, Colors),
    draw_tile(Ctx, Colors, Size, X, Y).

colors_map([], []).
colors_map([H|T], [HC|TC]) :-
    color_map(H, HC),
    colors_map(T, TC).

color_map(1, '#FFFFFF').
color_map(2, '#4444FF').
