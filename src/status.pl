:- module(status, [display_status/0, next_status/2]).

:- use_module(library).
:- use_module('../proscriptls_sdk/library/object'). % for >>/2.
:- use_module(game_model_tiles).
:- use_module(locations).
:- use_module(view_basics).
:- use_module(draw).

% phase: X,
% player: Y,
% next action/phase: Z

display_status :-
    display_status(Marker, Phase, Turn, Next),

    display_player(Turn),

    status_element('marker: ~w', [Marker], h2, MarkerElement),
    status_element('phase: ~w', [Phase], h2, PhaseElement),
    status_element('player: ~w', [Turn], h2, PlayerElement),

    Node >> [id -:> mosaic_status, innerHTML <:+ ""],
    !,

    append_dom_node_child(Node, MarkerElement),
    append_dom_node_child(Node, PhaseElement),
    append_dom_node_child(Node, PlayerElement),
    display_next_status(Next, Node).

display_status(Marker, Phase, Turn, Next) :-
    get_selection_marker(Marker),
    get_turn(Turn),
    get_game_phase(Phase),
    next_status(Phase, Next).

display_player(P) :-
    %number_codes(P, PCodes),
    %Node >> [id -:> mosaic_player],
    _Canvas >> [id -:> mosaic_player_canvas, getContext('2d') *:> Ctx],
    get_player_color(P, Color),
    draw_tile(Ctx, [Color, Color, Color, Color], 30, 0, 0).


status_element(Format, Args, Tag, Element) :-
    format(atom(PhaseAtom), Format, Args),
    atom_codes(PhaseAtom, PhaseAtomCodes),
    create_dom_element(Tag, Element),
    Element >+> innerText <: PhaseAtomCodes.

/*
<div>
next:
<ul>
<li>select tile, then build</li>
<li>select location, then build</li>
</ul>
</div>
*/
display_next_status(Next, Node) :-
    create_dom_element(div, Div),
    append_dom_node_child(Node, Div),
    create_dom_text_node("next:", NextText),
    append_dom_node_child(Div, NextText),
    create_dom_element(ul, Ul),
    append_dom_node_child(Div, Ul),
    display_next_status1(Next, Ul).

display_next_status1([], _).
display_next_status1([Action-Phase|T], Ul) :-
    status_element('select ~w, then ~w', [Action, Phase], li, Element),
    append_dom_node_child(Ul, Element),
    display_next_status1(T, Ul).

/*
build1: build, no locations.
build2: build, locations.
replace1: replace, no locations.
replace2: replace, locations.
rebuild1: rebuild, no locations.
rebuild2: rebuild, locations, singleton active hand, no mismatches
rebuild3: rebuild, locations, multiple active hand
rebuild4: rebuild, locations, singleton active hand, mismatches

build1 -- clicked tile --> build2 (new selected tile, new tile-specific locations) or (rotated tile, new legal locations)
build2 -- clicked tile --> build2 (new selected tile, new tile-specific locations)
build2 -- clicked location --> build1 (exist hand tiles) or transform

transform -- clicked edge --> replace1 (new locations, new replacements)

replace1 -- clicked tile --> replace2 (new legal locations) or (rotated tile, new legal locations)
replace2 -- clicked tile --> replace2 (new selected tile, new legal locations)
replace2 -- clicked location --> replace1 (one less replacement, updated board), rebuild1 (updated board)

rebuild1 -- clicked tile --> rebuild2,rebuild3,rebuild4 (new selected tile, new tile-specific locations)
rebuild2,rebuild3,rebuild4 -- clicked tile --> rebuild2,rebuild3,rebuild4 (new selected tile, new tile-specific locations) or (rotated tile, new legal locations)
rebuild2 -- clicked location --> transform
rebuild3 -- clicked location --> rebuild1
rebuild4 -- clicked location --> replace1
*/
next_status(build, [tile-build]) :-
    get_legal_positions([]).
next_status(build, [tile-build, location-LocationTransition]) :-
    get_legal_positions([_|_]),
    (get_hands(Hands),
     append_lists(Hands, [_])
      -> LocationTransition = transform
     ;
     LocationTransition = build
    ).
next_status(transform, [edge-replace]).
next_status(replace, [tile-replace]) :-
    get_legal_positions([]).
next_status(replace, [tile-replace, location-LocationTransition]) :-
    get_legal_positions([_|_]),
    (get_replacements([_])
      -> LocationTransition = rebuild
     ;
     LocationTransition = replace
    ).
next_status(rebuild, [tile-rebuild]) :-
    get_legal_positions([]).
next_status(rebuild, [tile-rebuild,location-LocationTransition]) :-
    get_legal_positions([_|_]),
    (get_hands(Hands),
     append_lists(Hands, [_,_|_])
      -> LocationTransition = rebuild
     ;
     get_mismatches([])
      -> LocationTransition = transform
     ;
     LocationTransition = replace
    ).
