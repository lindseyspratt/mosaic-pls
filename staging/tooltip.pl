
tooltip(Event, Ctx) :-
    dom_object_type(Event, Type),
    Event >+> type :> Subtype,
    writeln(tooltip(Event,Type, Subtype)),
    Event >> [pageX +:> PageX, pageY +:> PageY],
    dom_release_object(Event),
    tooltip(PageX, PageY, Ctx).

tooltip(PageX, PageY, Ctx) :-
    writeln(tooltip(PageX, PageY, Ctx)),
    number_codes(PageX, XCodes),
    number_codes(PageY, YCodes),
    append_lists([XCodes, "-", YCodes], TextCodes),
    atom_codes(Text, TextCodes),
    get_canvas_height(H),
    get_canvas_width(W),
    Ctx >*> clearRect(0, 0, W, H),
    draw_label(Ctx, PageX, PageY, Text).
