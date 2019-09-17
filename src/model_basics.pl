/**
 * gameModelBasics function creates an object that defines basic parameters of a game.
 * These attributes are accessed using these functions:
 * get_hand_color_ids : color ID sequences for all hands 0 through numberOfPlayers - 1.
 *
 * get_number_of_players : number of players in the game (2, 3, or 4).
 *
 * get_triangles_per_tile : number of triangles per tile, either 4 for square tiles or 6 for hexagonal tiles.
 * ---
 * The basics object also defines a number of utility functions:
 * rotate_right : rotate a color sequence 'right' moving the 0'th element to the end (K'th) position, shifting
 * all other items down one position.
 *
 * rotate_left : rotate a color sequence 'left' moving the last (K'th) element to the beginning (0'th) position,
 * shifting all other items up one position.
 *
 * board_hash_key_coords : convert a pair of coordinates to a unique string used as a lookup key. E.g. (2,3)
 * becomes 'x2y3'.
 *
 * tile_colors_match_constraint_colors : returns a true if there are no mismatches between a color constraint
 * sequence and a tile color sequence. The mismatch count of position offsets in a color constraint sequence
 * and a tile color sequence is incremented for a given position offset in the two sequences where
 * the color constraint value is >= 0 and the tile color ID is not equal to the color constraint ID.
 * In this definition any color constraint value < 0 matches any tile color ID.
 *
 * edge_neighbor_offset : maps an 'edge' identifer (0 to 3 for squares) to a dx/dy pair to find the neighbor tile
 * position across that edge from a given tile position. From a tile at grid position (1,1), the neighbor across
 * edge 0 has (dx,dy) of (0,-1). This gives the edge 0 neighbor grid position of (1, 0).
 *
 * values : The 'values' function creates a displayable-on-the-console object that is the labeled values
 * returned by the parameter-defining functions of the gameModelBasics object - get_hand_color_ids,
 * get_number_of_players, and get_triangles_per_tile.
 *
 * @param {Object} spec - specification of parameters for the basics of the game model.
 * @param {number} spec.numberOfPlayers
 * @param {number} spec.trianglesPerTile
 */

:- module(model_basics, [get_hand_color_ids/1, get_number_of_players/1, get_triangles_per_tile/1,
    rotate_right/2, rotate_left/2, board_hash_key_coords/3, tile_colors_match_constraint_colors/2,
    edge_neighbor_offset/2, values/1]).

    /**
	 * get_hand_color_ids returns an array of color ID sequences for hands 0 through numberOfPlayers-1,
	 * where color ID is an integer mapped to a color and hand
     * with offset K (0 to number of players - 1) is for player K.
	 * Each color ID sequence is used to define a tile by specifying the colors of the
	 * triangles of that tile.
     * @type {function(): number[][][]}
     */
	that.get_hand_color_ids = get_hand_color_ids;

    /**
	 * get_number_of_players returns number of players in the game (2, 3, or 4).
     * @type {function(): number}
     */
	that.get_number_of_players = get_number_of_players;

    /**
	 * get_triangles_per_tile returns number of triangles per tile,
	 * either 4 for square tiles or 6 for hexagonal tiles.
     * @type {function(): number}
     */
	that.get_triangles_per_tile = get_triangles_per_tile;

    /**
	 * rotate_right creates a version of the input color sequence
	 * that is rotated 'right' moving the 0'th element to the end (K'th) position, shifting
     * all other items down one position.
     * @type {function( number[]): number[]}
     */
	that.rotate_right = rotate_right;

    /**
	 * rotate_left creates a version of the input color sequence
     * that is rotated 'left' moving the last (K'th) element to the beginning (0'th) position,
     * shifting all other items up one position.
     * @type {function( number[]): number[]}
     */
	that.rotate_left = rotate_left;

    /**
     * board_hash_key_coords funtion converts a pair of coordinates to a unique string used as a lookup key.
	 * E.g. (2,3) becomes 'x2y3'.
     * @type {function(number, number): string}
     */
	that.board_hash_key_coords = board_hash_key_coords;

    /**
	 * tile_colors_match_constraint_colors function returns a true if there are no mismatches between a color constraint
     * sequence and a tile color sequence. The mismatch count of position offsets in a color constraint sequence
     * and a tile color sequence is incremented for a given position offset in the two sequences where
     * the color constraint value is >= 0 and the tile color ID is not equal to the color constraint ID.
     * In this definition any color constraint value < 0 matches any tile color ID.
     * @type {function(number[], number[]): boolean}
     */
	that.tile_colors_match_constraint_colors = tile_colors_match_constraint_colors;

    /**
	 * edge_neighbor_offset function maps an 'edge' identifer (0 to 3 for squares) to a dx/dy pair to find the neighbor tile
     * position across that edge from a given tile position. From a tile at grid position (1,1), the neighbor across
     * edge 0 has (dx,dy) of (0,-1). This gives the edge 0 neighbor grid position of (1, 0).
     * @type {function(number): {dx: number, dy: number}}
     */
 	that.edge_neighbor_offset = edge_neighbor_offset;

    /**
     * The 'values' function creates a displayable-on-the-console object that is the labeled values
     * returned by the parameter-defining functions of the gameModelBasics object - get_hand_color_ids,
     * get_number_of_players, and get_triangles_per_tile.
     * @type {function(): {get_hand_color_ids : number[], get_number_of_players:number, get_triangles_per_tile:number}}
     */
	that.values = values;

mosaic.gameModelBasics = function(spec) {
	var that = {};
	var numberOfPlayers = spec.numberOfPlayers;
	var trianglesPerTile = spec.trianglesPerTile;

    /**
     * handColorIDSequences is set to an array of hands, one for each player.
	 * Each hand is an array of color ID sequences.
	 * These arrays of color ID sequences are used to define sets of tiles.
	 * @type {number[][][]}
     */
    var handColorIDSequences;

    /**
	 * build_base_sequences constructs an array of color sequences that defines the basic collection of
	 * tiles used to construct a hand of tiles. This basic collection contains various one and two color
	 * tiles using the 'a' and 'b' input colors.
     * @param {number} a - color ID
     * @param {number} b - color ID
     * @returns {number[][]} baseSequence
     */
	var build_base_sequences = function(a, b) {
		if(spec.trianglesPerTile===4) {
			return [[a,a,a,a], [b,b,b,b], [b,a,a,a], [a,b,b,b], [b,b,a,a], [a,a,b,b], [b,a,b,a], [a,b,a,b]];
		} else if(spec.trianglesPerTile===6) {
			return [[a,a,a,a,a,a], [b,b,b,b,b,b], [b,a,a,a,a,a], [a,b,b,b,b,b],
		        [b,b,a,a,a,a], [a,a,b,b,b,b],
		        [b,b,b,a,a,a], [a,a,a,b,b,b],
		        [b,a,b,a,a,a], [a,b,a,b,b,b],
		        [b,a,b,a,b,a], [a,b,a,b,a,b]];
		}
	};

    /**
	 * build_sequences creates an array of hands.
     * @param {number} numberOfPlayers - number of players for which to create hands.
     * @returns {number[][][]} hands
     */
	var build_sequences = function(numberOfPlayers) {
		if(numberOfPlayers===2) {
			return build_sequences_2();
		} else if(numberOfPlayers===3) {
			return build_sequences_3();
		} else if(numberOfPlayers===4) {
			return build_sequences_4();
		}
	};

    /**
	 * build_sequences_2 creates a two-player array of color ID sequences for colors 0 and 1.
     * @returns {number[][][]} baseSequence
     */
	function build_sequences_2() {
        var pair01 = build_base_sequences(0, 1);
        return [pair01, pair01]
    }

    /**
     * build_sequences_3 creates a three-player array of color ID sequences for colors 0, 1, and 2.
     * @returns {number[][][]} baseSequence
     */
	function build_sequences_3() {
        var pair01 = build_base_sequences(0, 1);
        var pair02 = build_base_sequences(0, 2);
        var pair12 = build_base_sequences(1, 2);

        var player0 = pair01.concat(pair02);
        var player1 = pair01.concat(pair12);
        var player2 = pair02.concat(pair12);

        return [player0, player1, player2];
	}


    /**
     * build_sequences_4 creates a four-player array of color ID sequences for colors 0, 1, 2, and 3.
     * @returns {number[][][]} baseSequence
     */
	function build_sequences_4() {
        var pair01 = build_base_sequences(0, 1);
        var pair02 = build_base_sequences(0, 2);
        var pair03 = build_base_sequences(0, 3);
        var pair12 = build_base_sequences(1, 2);
        var pair13 = build_base_sequences(1, 3);
        var pair23 = build_base_sequences(2, 3);

        var player0 = pair01.concat(pair02, pair03);
        var player1 = pair01.concat(pair12, pair13);
        var player2 = pair02.concat(pair12, pair13);
        var player3 = pair03.concat(pair13, pair23);

        return [player0, player1, player2, player3];
	}

	handColorIDSequences = build_sequences(spec.numberOfPlayers);

	var get_number_of_players = function() {
		return numberOfPlayers;
	};

	var get_triangles_per_tile = function() {
		return trianglesPerTile;
	};

 	var get_hand_color_ids = function() {
		return handColorIDSequences;
	};

	var rotate_right = function(tileColors) {
	    var newTileColors = tileColors.slice();
	    newTileColors.unshift(newTileColors.pop());
	    return newTileColors;
	};

	var rotate_left = function(tileColors) {
	    var newTileColors = tileColors.slice();
	    newTileColors.push(newTileColors.shift());
	    return newTileColors;
	};

	var board_hash_key_coords = function (gridX, gridY) {
	    return 'x' + gridX + 'y' + gridY;
	};

	var tile_colors_match_constraint_colors = function (tileColors, colorConstraints) {
		var mismatches = tile_colors_mismatch_constraint_colors(tileColors, colorConstraints);
		return (mismatches === 0);
	};

	var tile_colors_mismatch_constraint_colors = function (tileColors, colorConstraints) {
		var mismatches = 0;
	    for (var positionOffset = 0; positionOffset < trianglesPerTile; positionOffset++) {
	        var colorOffset = colorConstraints[positionOffset];
	        if (colorOffset >= 0 && colorOffset !== tileColors[positionOffset]) {
	        	mismatches++;
	        }
	    }
	    return mismatches;
	};

	var edge_neighbor_offset = function (edge) {
		if(trianglesPerTile===4) {
			return edge_square_neighbor_offset(edge);
		} else if(trianglesPerTile === 6) {
            return edge_hex_neighbor_offset(edge);
		} else
			throw 'Invalid trianglesPerTile value. It must be 4 or 6. trianglesPerTile=' + trianglesPerTile + '.';
	};

    /**
	 * edge_square_neighbor_offset function maps an edge identifier 0 through 3
	 * to a dX/dY displacement to shift from one tile square grid X/Y position
	 * to a neighbor tile square grid X/Y position.
     * @param edge
     * @returns {{dx: number, dy: number}}
     */
    var edge_square_neighbor_offset = function (edge) {
        var dx = 0;
        var dy = 0;
        if(edge === 0) {
            dy = -1;
        } else if(edge === 1) {
            dx = 1;
        } else if(edge === 2) {
            dy = 1;
        } else if(edge === 3) {
            dx = -1;
        } else {
            throw "invalid edge value: " + edge;
        }
        return {"dx":dx, "dy":dy};
    };

    /**
	 * edge_hex_neighbor_offset function maps an edge identifier 0 through 5
	 * to an dX/dY displacement to shift from one tile hexagon grid X/Y
	 * position to a neighbor tile hexagon grid X/Y position.
	 *
	 * The hexagon grid is defined by two axes at a 120 degree angle.
	 * The Y axis is vertical and the X axis is 120 degrees from the Y
	 * axis (instead of 90 degrees).
	 *
     * @param edge
     * @returns {{dx: number, dy: number}}
     */
    var edge_hex_neighbor_offset = function (edge) {
        var dx = 0;
        var dy = 0;
        if(edge === 0) {
            dy = -1;
        } else if(edge === 1) {
            dx = 1;
        } else if(edge === 2) {
        	dx = 1;
            dy = 1;
        } else if(edge === 3) {
            dx = 1;
        } else if(edge === 4) {
            dx = -1;
        } else if(edge === 5) {
            dx = -1;
            dy = -1;
        } else {
            throw "invalid edge value: " + edge;
        }
        return {"dx":dx, "dy":dy};
    };

    var values = function() {
		var display = {};
		display.get_hand_color_ids=get_hand_color_ids();
		display.get_number_of_players=get_number_of_players();
		display.get_triangles_per_tile=get_triangles_per_tile();
		return display;
	};

    /**
	 * get_hand_color_ids returns an array of color ID sequences for hands 0 through numberOfPlayers-1,
	 * where color ID is an integer mapped to a color and hand
     * with offset K (0 to number of players - 1) is for player K.
	 * Each color ID sequence is used to define a tile by specifying the colors of the
	 * triangles of that tile.
     * @type {function(): number[][][]}
     */
	that.get_hand_color_ids = get_hand_color_ids;

    /**
	 * get_number_of_players returns number of players in the game (2, 3, or 4).
     * @type {function(): number}
     */
	that.get_number_of_players = get_number_of_players;

    /**
	 * get_triangles_per_tile returns number of triangles per tile,
	 * either 4 for square tiles or 6 for hexagonal tiles.
     * @type {function(): number}
     */
	that.get_triangles_per_tile = get_triangles_per_tile;

    /**
	 * rotate_right creates a version of the input color sequence
	 * that is rotated 'right' moving the 0'th element to the end (K'th) position, shifting
     * all other items down one position.
     * @type {function( number[]): number[]}
     */
	that.rotate_right = rotate_right;

    /**
	 * rotate_left creates a version of the input color sequence
     * that is rotated 'left' moving the last (K'th) element to the beginning (0'th) position,
     * shifting all other items up one position.
     * @type {function( number[]): number[]}
     */
	that.rotate_left = rotate_left;

    /**
     * board_hash_key_coords funtion converts a pair of coordinates to a unique string used as a lookup key.
	 * E.g. (2,3) becomes 'x2y3'.
     * @type {function(number, number): string}
     */
	that.board_hash_key_coords = board_hash_key_coords;

    /**
	 * tile_colors_match_constraint_colors function returns a true if there are no mismatches between a color constraint
     * sequence and a tile color sequence. The mismatch count of position offsets in a color constraint sequence
     * and a tile color sequence is incremented for a given position offset in the two sequences where
     * the color constraint value is >= 0 and the tile color ID is not equal to the color constraint ID.
     * In this definition any color constraint value < 0 matches any tile color ID.
     * @type {function(number[], number[]): boolean}
     */
	that.tile_colors_match_constraint_colors = tile_colors_match_constraint_colors;

    /**
	 * edge_neighbor_offset function maps an 'edge' identifer (0 to 3 for squares) to a dx/dy pair to find the neighbor tile
     * position across that edge from a given tile position. From a tile at grid position (1,1), the neighbor across
     * edge 0 has (dx,dy) of (0,-1). This gives the edge 0 neighbor grid position of (1, 0).
     * @type {function(number): {dx: number, dy: number}}
     */
 	that.edge_neighbor_offset = edge_neighbor_offset;

    /**
     * The 'values' function creates a displayable-on-the-console object that is the labeled values
     * returned by the parameter-defining functions of the gameModelBasics object - get_hand_color_ids,
     * get_number_of_players, and get_triangles_per_tile.
     * @type {function(): {get_hand_color_ids : number[], get_number_of_players:number, get_triangles_per_tile:number}}
     */
	that.values = values;

	return that;
};
