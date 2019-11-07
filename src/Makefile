PLS_DIST = ../proscriptls_sdk/dist
PLS_LIB = ../proscriptls_sdk/library
PLS_NODE_TOOLS = ../proscriptls_sdk/node_tools
#PLS_DIST = ../../proscriptls/dist
#PLS_LIB = ../../proscriptls/library

all:	proscriptls_state_tiles.js

.PHONY: all clean

clean:
		rm -f proscriptls_state_tiles.js

proscriptls_state_tiles.js:		${PLS_DIST}/proscriptls_engine_for_node.js tiles.pl library.pl model_basics.pl view_basics.pl\
								tile_model.pl game_model_tiles.pl tile_view.pl game_view_tiles.pl draw.pl locations.pl location_model.pl\
								${PLS_LIB}/data_predicates.pl ${PLS_LIB}/object.pl ${PLS_NODE_TOOLS}/node_compile.js
		node ${PLS_NODE_TOOLS}/node_compile.js `pwd`/tiles.pl `pwd`/proscriptls_state_tiles.js
		node ${PLS_NODE_TOOLS}/node_goal.js `pwd`/proscriptls_state_tiles.js true