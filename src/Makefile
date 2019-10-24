PLS_DIST = ../proscriptls_sdk/dist
PLS_LIB = ../proscriptls_sdk/library

all:	proscriptls_state_tiles.js

.PHONY: all clean

clean:
		rm -f proscriptls_state_tiles.js

proscriptls_state_tiles.js:		${PLS_DIST}/proscriptls_for_compile.js tiles.pl library.pl model_basics.pl view_basics.pl\
								tile_model.pl game_model_tiles.pl tile_view.pl\
								${PLS_LIB}/data_predicates.pl ${PLS_LIB}/object.pl ${PLS_DIST}/node_compile.js
		node ${PLS_DIST}/node_compile.js `pwd`/tiles.pl `pwd`/proscriptls_state_tiles.js