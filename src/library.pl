:- module(library, [nth0/3, nth1/3, append_lists/2, dom_page_offset/3,
        add_element/3, disjoint/2, intersect/2, seteq/2,
        symdiff/3, perm2/4, remove_dups/2, rev/2, sumlist/2,
        subtract/3, select/4
]).

:- ensure_loaded('../proscriptls_sdk/library/listut'). % for nth1/3, nth0/3
:- ensure_loaded('../proscriptls_sdk/library/listut2'). % for append_lists/2
:- ensure_loaded('../proscriptls_sdk/library/setutl'). % for subtract/3
:- ensure_loaded('../proscriptls_sdk/library/dom'). % for dom_page_offset/3
