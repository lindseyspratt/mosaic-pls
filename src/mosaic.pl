/*
 * Game has many states.
 * For each state, there is a single interpretation of a mouse-click, a user's selection of an interface item.
 *
 * Game items:
 * Tile - the basic playing element. All of the tiles in the game are the same shape. The tiles
 * are originally square. They could be any shape the tiles a plane, e.g. hexagonal.
 *
 * Triangle - a portion of a tile defined by two corners of the tile that share an edge and the center
 * of the tile. A square tile has four triangles. A hexagonal tile has six triangles.
 *
 * Color - there are as many colors used in the definition of the tiles as there are players in the game.
 * Each player has a unique color associate with that player. Each triangle is a single color.
 * A tile is composed of one or two colors.
 *
 * Hand - a possibly empty collection of tiles associate with a particular player.
 *
 * Initial hand - a collection of tiles at the start of the game. Each player has the same set of
 * tile patterns OR
 * a player has a set of tile patterns symmetric with the other player sets but with that player's
 * color dominating.
 *
 * Board - tiles that are played from hands. There are constraints on how tiles in the board are
 * configured regarding adjacency, color matching at adjacent tile edges, and on the overall shape
 * of the edge of the board as defined by the played tiles.
 *
 * Selected Tile - a tile is indicated as selected (e.g. a small '+' in the middle of the tile)
 * after the user has selected (e.g. right-clicked on it with a mouse) that tile for use in a game action.
 * A selected tile is in the hand or board.
 *
 * Selected Tile Orientation - The orientation of a selected tile is rotated
 * 90 degrees clockwise if it is "re-selected" (e.g. right-clicked again without having right-clicked
 * another tile in the interim). If another tile is selected (rather than placing the current selected tile)
 * then the orientation of the current selected tile must be restored to its original orientation before
 * setting up the new selected tile.
 *
 * Possible Location - a tile-shaped location in or next to the board. The location must be empty:
 * there must not be a tile in this location. A possible location is dictated by the game rules:
 * a tile must be selected to fill a possible location. It may be represented by a simple
 * outline of the tile shape (e.g. square or hexagon).
 *
 * Usable Location - A usable location is a possible location that the selected tile in its
 * selected orientation can validly move to. It may be represented by a solid non-player-colored
 * shape (e.g. square or hexagon). A usable location can only appear in the UI
 * if there is a single selected tile. The usable location is "usable" as
 * the target of a valid move of that selected tile.
 * When a usable location is selected by the user then the
 * current selected tile is moved from its current location (in a hand)
 * to that selected usable location. The move of a selected tile to a usable location preserves
 * that selected tile's orientation in the new location.
 * Changing the orientation of the selected tile can change the usable locations for the board.
 *
 * Board Joint - the adjacent edges of two adjacent tiles in the board. Since a constraint of a
 * stable or resolved board is that the adjacent triangles of two adjacent tiles must have the
 * same color, then there is one color for any tile joint in a stable board. The basic action
 * of the second part of the game is the 'joint color change'.
 *
 * Replacement Tile - a tile that is a valid replacement for another tile. One action in which
 * a replacement tile is used is the 'joint color change'. In this action, the original tiles for
 * the selected joint must be replaced. The replacement tiles in the board are all of those
 * tiles that have the appropriate pattern of triangle colors to validly complete the action.
 *
 * Selectable Tile - a tile that may be selected to continue a game action. It is indicated
 * as selectable (e.g. by a small circle in the center of the tile). In the case of the
 * 'joint color change' action the valid replacement tiles are the selectable tiles. In the
 * case of a mismatch resolution the tiles that best solve the mismatch are the selectable tiles.
 *
 * Active Hand Tile - a tile in the hand of the player that is active. This is represented
 * distinctively from the hand tiles of the inactive player(s). E.g. an active tile has the
 * same colors as the inactive tiles but is brighter. An active hand tile may be selected if
 * there are no selectable tiles otherwise specifically indicated.
 *
 * Active Player - an indicator on the board shows the active player. The players take turns
 * being active, one after the other, no player gets another turn until every player has
 * had a turn, and always in the same sequence.
 *
 * Board Score - the score for each player is presented in some fashion that indicates which
 * score is for which player (e.g. each score is shown in the color of the respective player).
 * The scores are updated whenever the board becomes stable (or 'resolved').
 * -----
 *
 * Turns:
 * There is a major turn sequence and minor turn sequences. The Joint Color Change Initiation action
 * is a major turn action.
 *
 * Game Actions:
 *
 * Joint Color Change Initiation - a player selects a tile joint that is not that player's
 * color and replaces the two tiles for that joint with two other tiles that differ only
 * in the triangles for the joint being the player's color. The selected joint cannot
 * be the same joint selected by another player since the selecting player's last turn.
 * The two replacement tiles
 * are taken from the board; this may leave the board in violation of the shape constraints.
 * Particularly, it may leave a three or four sided hole in the board. These holes are
 * filled using the original joint tiles by the the player initiating the change. This
 * 'filling' must be done so that there is no color mismatch if possible, otherwise the
 * color mismatching must be minimized.
 *
 * Mismatch Resolution - If there is a color mismatch in the board (such as after a
 * joint color change initiation) then the next minor turn sequence player must resolve
 * it. The mismatched tiles are removed from the board to the resolving player's hand.
 * The resolving player selects from the tiles that are the color-resolved versions of
 * the mismatched tiles and moves them to the mismatched tiles original locations.
 * The selected tile cannot be one that cause the mismatch if there is any other
 * possible tile. This may leave a hole in the board where the resolving tile was located originally.
 * The remaining tiles in the resolving player's hand are placed in the board - forced
 * to the holes in the board, if any. If there are any mismatches at the end of this
 * process then play turn moves to the next minor sequence player who executes
 * a mismatch resolution action. This process of repeating resolutions continues until
 * the board is stable (or resolved): it has no mismatches (or holes).
 *
 * Build Placement - Place a tile in the board from a hand. The tile must be added in
 * a location that has at least one edge adjacent tile and two edge or corner
 * adjacent tiles. The selected tile must be placed edge adjacent to the previously
 * placed tile if possible for that selected tile. The selected tile must match the
 * colors of its adjacent tile edges. The placed tile cannot have a board tile that
 * is one empty tile location away from any of its edges.
 *
 * ---
 *
 * Game State:
 *
 * turn player sequence
 * major turn player
 * minor turn player
 *
 * all game tiles list
 * 	for each tile
 * 		tile ID (offset in 'all game tiles list'?)
 * 		tile triangle color sequence
 * 			colors in clockwise sequence, 0 offset is 'up' or 12 o'clock.
 * 		current hand/board location
 * 			x/y tile offset in container - hand or board (display geometry location calculated from this)
 * 		current rotation
 * 		container (hand or board)
 * 		selectable flag
 * 		selected flag
 * hands tiles
 * 	for each player hand
 * 		list of tile IDs
 * board tiles
 * 	list of tile IDs
 *
 * target locations in or near board tiles
 * 	for each target location
 * 		x/y location offset in board
 * 		color sequence constraint
 * 			sequence is clockwise, 0 offset is 'up' or 12 o'clock.
 * 			zero, one, or more constraints in the sequence may be 'any' (i.e. unconstrained) instead of a specific color.
 *
 */
