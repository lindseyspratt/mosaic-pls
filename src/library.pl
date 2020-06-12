:- module(library, [nth0/3, nth1/3, nmember/3,
        append_lists/2, dom_page_offset/3,
        add_element/3, disjoint/2, intersect/2, seteq/2,
        symdiff/3, perm2/4, remove_dups/2, rev/2, sumlist/2,
        subtract/3, select/4, sort_cut/2,
        put_assoc/4, get_assoc/3, assoc_to_list/2, map_assoc/3,
        balance_assoc/2, assoc_stat/2, difference/3,
        fail_save/1
]).

:- ensure_loaded('../proscriptls_sdk/library/listut'). % for nth1/3, nth0/3
:- ensure_loaded('../proscriptls_sdk/library/listut2'). % for append_lists/2
:- ensure_loaded('../proscriptls_sdk/library/setutl'). % for subtract/3
:- ensure_loaded('../proscriptls_sdk/library/dom'). % for dom_page_offset/3
:- ensure_loaded(assoc).

:- meta_predicate(fail_save((:))).

:- dynamic(fail_save_counter/1).
:- dynamic(fail_saved/2).

sort_cut(In, Out) :-
    sort(In, Out),
    !.

difference(Minuend, Subtrahend, Difference) :-
    setof(X, (member(X, Minuend), \+member(X, Subtrahend)), Difference)
      -> true
    ;
    Difference = [].

increment_fail_save_counter(ID) :-
    (retract(fail_save_counter(Old))
      -> ID is Old + 1
    ;
     ID = 1
    ),
    asserta(fail_save_counter(ID)).

fail_save(Goal) :-
    increment_fail_save_counter(ID),
    fail_save(ID, Goal).

fail_save(ID, Goal) :-
    once(Goal),
    asserta(fail_saved(ID, Goal)),
    fail.
fail_save(ID, Goal) :-
    once(retract(fail_saved(ID, Goal))), % the ID is unique, so only one answer is possible. The once/1 eliminates an unnecessary checkpoint frame.
    ID = _,
    Goal = _.
%    write_term(retract(fail_saved(ID, Goal)), [max_depth(8)]),
%    writeln('').
