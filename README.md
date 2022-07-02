# mosaic-pls
Mosaic game implemented using ProscriptLS, v 0.1.0.

The game is documented in src/doc.html, which is linked from index.html and src/mosaic.html.

This implementation demonstrates use of Canvas 2D elements and user interactions all handled through Prolog.
Also there is a 'database' facility (implemented in Prolog) to handle game state and support undo and redo.

Running the game is done through index.html and src/mosaic.html.

mosaic.html relies on src/proscriptls_state_tiles.js which is built from the src/*.pl files running src/Makefile.

doc.html relies on src/proscriptls_state_doc.js which is also built by src/Makefile.

The game implementation (mosaic.html) relies on ProscriptLS 1.5.9 implementation available through the jsdelivr.net CDN at
https://cdn.jsdelivr.net/gh/lindseyspratt/proscriptls@1.5.9/dist/proscriptls_engine.js
This same version of ProscriptLS is in proscriptls_sdk, which is used by the src/Makefile to compile mosaic.

