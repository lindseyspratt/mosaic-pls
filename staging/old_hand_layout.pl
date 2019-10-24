invoke :-
    get_number_of_players(NumberOfPlayers),
    initial_hands_expanded(NumberOfPlayers, Hands),
    setup_hands(Hands, TileIDs).

setup_hands([], []).
setup_hands([_+HandTiles|T], IDs) :-
    %writeln(setting_up_hand(HandTiles)),
    setup_hand(HandTiles, IDs, IDTail),
    setup_hands(T, IDTail).

setup_hand([], Tail, Tail).
setup_hand([x(X, Y, Size,GridX,GridY)-ID|T], [ID|OtherIDs], IDTail) :-
    %writeln(setup_hand_tile(x(H,GridX,GridY)-ID)),
    update_grid_x(ID, _, GridX),
    update_grid_y(ID, _, GridY),
    %assert_data(H, ID),
    create_tile_view(ID, X, Y, Size),
    setup_hand(T, OtherIDs, IDTail).

hand_origin(1).

initial_hands_expanded(NumberOfPlayers, ExpandedHands) :-
    initial_hands(NumberOfPlayers, Hands),
    expand_hands(Hands, ExpandedHands).

expand_hands([], []).
expand_hands([H|T], [EH|ET]) :-
    expand_hand(H, EH),
    expand_hands(T, ET).

expand_hand(ID+BriefTiles, ID+ExpandedTiles) :-
    get_hand_tile_size(Size),
    expand_brief_tiles(BriefTiles, Size, ExpandedTiles).

expand_brief_tiles([], _, []).
expand_brief_tiles([H|T], Size, [EH|ET]) :-
    expand_brief_tile(H, Size, EH),
    expand_brief_tiles(T, Size, ET).

% [x, y,bx,by,size,colors,container]
expand_brief_tile(t(BoardX, BoardY, TileID),
        Size,
        x(X, Y, Size, BoardX, BoardY)-TileID) :- % make the TileID and ModelID the same.
    X is BoardX * Size,
    Y is BoardY * Size.

/*
Fill in gridX and gridY values for the tile models created by game_model_tiles:build_tiles/0
such that the tiles for a hand are grouped together in a vertical rectangle 2 columns wide and 4
rows deep and these two hand rectangles are placed one above the other with a small space
separating them.
*/
initial_hands(2, [1+Player1Tiles, 2+Player2Tiles]) :-
    hand_origin(Origin1),
    Origin2 is Origin1 + 5,
    get_hands([ModelHand1, ModelHand2]),
    place_hand(ModelHand1, 1, Origin1, 0, 4, Player1Tiles),
    length(Player1Tiles, Player1TilesLength),
    place_hand(ModelHand2, 1, Origin2, Player1TilesLength, 4, Player2Tiles).

% place_hand(Hand, BaseCol, BaseRow, PlacedSoFar, MaxColumns, MaxRows, PlacedHand).
place_hand(AbstractHand, BaseCol, BaseRow, PlacedSoFar, MaxRows, PlacedHand) :-
    place_hand(AbstractHand, BaseCol, BaseRow, PlacedSoFar, PlacedSoFar, MaxRows, PlacedHand).

place_hand([], _BaseCol, _BaseRow, _Counter, _InitialCounter, _MaxRows, []).
place_hand([H|T], BaseCol, BaseRow, Counter, InitialCounter, MaxRows, [HP|TP]) :-
    place_hand1(H, BaseCol, BaseRow, Counter, InitialCounter, MaxRows, HP),
    CounterNEXT is Counter + 1,
    place_hand(T, BaseCol, BaseRow, CounterNEXT, InitialCounter, MaxRows, TP).

place_hand1(H, BaseCol, BaseRow, Counter, InitialCounter, MaxRows, t(Col, Row, H)) :-
    RowIncrement is (Counter-InitialCounter) mod MaxRows,
    ColIncrement is (Counter-InitialCounter) // MaxRows,
    Col is BaseCol + ColIncrement,
    Row is BaseRow + RowIncrement.
