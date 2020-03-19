:- module(agent, [init_agent/0, retract_agent/0, agent/2]).

:- use_module(library).
:- use_module('../proscriptls_sdk/library/data_predicates').
:- use_module(game_model_tiles).

:- initialization(initdyn).

initdyn :-
    data_predicate_dynamics([
        data_predicates(ag, data, [ephemeral],
            [clickHistory,historyPhase])]).

init_agent :-
    assert_data(ag([], none), 1).

retract_agent :-
    retract_all_data(data).

/*
An agent can be a player of Mosaic.
It makes valid "clicks" during its turn.
*/

agent(Clicks, Action) :-
    trim_clicks(Clicks, TrimmedClicks),
%    wam_duration(Duration),
    length(TrimmedClicks, Length),
%    Pick is (round(Duration) mod Length),
    random_between(1, Length, Pick),
    nth1(Pick, TrimmedClicks, Action),
    add_click(Action).

add_click(Click) :-
    data_default_id(ID),
    update_data_clickHistory(ID, Old, [Click|Old]).

trim_clicks(ClicksIn, ClicksOut) :-
    get_game_phase(GamePhase),
    (data_historyPhase(GamePhase)
      -> data_clickHistory(History),
         difference(ClicksIn, History, ClicksTrim),
         (ClicksTrim = []
           -> ClicksOut = ClicksIn
         ;
         ClicksOut = ClicksTrim
         )
    ;
    data_default_id(ID),
    set_data_historyPhase(ID, GamePhase),
    set_data_clickHistory(ID, []),
    ClicksOut = ClicksIn
    ).

