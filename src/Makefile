PLS_DIST = ../proscriptls_sdk/dist

all:	proscriptls_state_tiles.js

.PHONY: all clean

clean:
		rm -f proscriptls_state_tiles.js

proscriptls_state_tiles.js:  ${PLS_DIST}/proscriptls_for_compile.js tiles.pl library.pl model_basics.pl ${PLS_DIST}/node_compile.js
		node ${PLS_DIST}/node_compile.js `pwd`/tiles.pl `pwd`/proscriptls_state_tiles.js