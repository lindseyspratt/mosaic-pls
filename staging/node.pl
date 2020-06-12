:- module(node, [create_node/2, get_node_term/2]).

:- initialization(initdyn).

initdyn :-
    data_predicate_dynamics(
        [data_predicates(nd, data, [ephemeral],
            [term])
        ]).

retract_nodes :-
    retract_all_data(data).

create_node(ID, Term) :-
    assert_data(nd(Term), ID).

get_node_term(ID, Term) :-
    data_term(ID, Term).