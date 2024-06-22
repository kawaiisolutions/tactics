:- use_module(library(lists)).
:- use_module(library(dcgs)).
:- use_module(library(format)).

:- use_module(world).
:- use_module(unit).

%% effect(+Effect, +BeforeState, -AfterState, -Cues).

effect(tick, State0, State, [tick]) :-
	maplist(tick_unit, State0, State1),
	keysort(State1, State2),
	reverse(State2, State).

effect(next_turn, S0, S, Cs) :-
	next_turn(S0, S, Cs).

effect(begin_turn, [CT-Unit0|State], [CT-Unit|State], [focus_unit(ID)]) :-
	unit_with_status(-wait, Unit0, Unit1),
	unit_with_status(-acted, Unit1, Unit2),
	unit_with_status(-moved, Unit2, Unit3),
	unit_with_mp(+1, Unit3, Unit),
	unit_id(Unit, ID).

effect(move(To), [CT0-Unit0|State], [CT-Unit|State], [move_unit(ID, To)]) :-
	pos(To),
	ct_cost(move, Cost),
	CT0 >= Cost,
	CT is CT0 - Cost,
	can_move([CT0-Unit0|State], Unit0, To),
	unit_with_pos(To, Unit0, Unit1),
	unit_with_status(+moved, Unit1, Unit),
	unit_id(Unit, ID).

effect(end_turn, [CT0-Unit0|State], [CT-Unit|State], []) :-
	\+unit_has_status(wait, Unit0),
	unit_with_status(+wait, Unit0, Unit),
	ct_cost(end_turn, Cost),
	CT is max(CT0 - Cost, 0).

effect(attack(Target), [CT0-Unit0|State0], [CT-Unit|State], [attack(ID, Target), damage(Target, Damage)]) :-
	unit_id(Unit, ID),
	select_unit(Target, VCT-Victim0, State0, State1),
	unit_pos(Victim0, TargetPos),
	can_attack(Unit0, TargetPos),
	attack_damage(Unit0, Victim0, Damage),
	ct_cost(attack, Cost),
	CT0 >= Cost,
	CT is CT0 - Cost,
	unit_with_status(+acted, Unit0, Unit),
	unit_with_hp(-Damage, Victim0, Victim1),
	unit_hp(Victim1, HP),
	(  HP =< 0
	-> unit_with_status(+dead, Victim1, Victim)
	;  Victim = Victim1	
	),
	sort_state([VCT-Victim|State1], State).

ct_cost(move, 20).
ct_cost(attack, 20).
ct_cost(end_turn, 60).

can_do(move, State) :-
	current_unit(State, Unit),
	\+ \+can_move(State, Unit, _).
can_do(attack, State) :-
	current_unit(State, Unit),
	\+ \+can_attack(Unit, _).
can_do(end_turn, State) :-
	current_unit(State, Unit),
	\+unit_has_status(wait, Unit).

possible_actions(State, Actions) :-
	findall(Action, can_do(Action, State), Actions).

should_pass(State) :-
	possible_actions(State, [end_turn]).

menu(State, Actions) :- possible_actions(State, Actions).

next_turn(State0, State, Cues) :-
	next_turn_(State0, State, Cues),
	\+should_pass(State).
next_turn(State0, State, Cues) :-
	next_turn_(State0, State1, Cues1),
	should_pass(State1),
	effect(end_turn, State1, State2, Cues2),
	next_turn(State2, State, Cues3),
	phrase((Cues1, Cues2, Cues3), Cues).

next_turn_(State0, State, Cues) :-
	next_turn_tick_(State0, State1, Cues0),
	effect(begin_turn, State1, State, Cues1),
	append(Cues0, Cues1, Cues).
next_turn_tick_(State0, State, Cues) :-
	effect(tick, State0, State1, Cues0),
	[CT-_|_] = State1,
	(  CT >= 100
	-> State = State1, Cues = Cues0
	;  next_turn_(State1, State, Cues1), append(Cues0, Cues1, Cues)
	).

tick(State0, State, Cues) :-
	effect(tick, State0, State, Cues).

end_turn(State0, State, Cues) :-
	effect(end_turn, State0, State, Cues).

move(To, State0, State, Cues) :-
	effect(move(To), State0, State, Cues).

attack(Target, State0, State, Cues) :-
	effect(attack(Target), State0, State, Cues).

attack_damage(Unit, _Victim, Damage) :-
	unit_weapon(Unit, weapon(_, Min, Max)),
	random_between(Min, Max, Damage).

current_turn(State, Team) :-
	current_unit(State, Unit),
	unit_team(Unit, Team).
current_unit([_-Unit|_], Unit).

select_unit(ID, CT-Unit, State0, State) :-
	unit_id(Unit, ID),
	once(select(CT-Unit, State0, State)).

sort_state(State0, State) :-
	keysort(State0, State1),
	reverse(State1, State).

match_status(State, Status) :-
	once(match_status_(State, Status)).
match_status_(State, win(Team)) :-
	findall(T, (member(_-U, State), unit_team(U, T), \+unit_has_status(dead, U)), Teams0),
	sort(Teams0, [Team]).
match_status_(State, input(Team)) :-
	current_unit(State, Unit),
	unit_team(Unit, Team).
match_status_(_, draw).

test :-
	begin(42, State, _),
	run([move(4/5), end_turn,
		next_turn, end_turn,
		next_turn, attack(1), move(3/4), end_turn,
		next_turn, move(2/2), end_turn,
		next_turn], State, _, Cues),
	write(Cues),
	!.

begin(Seed, State, Cues) :-
	srandom(Seed),
	Unit1 = unit(1, red, soldier, 1/1, 10/10, 5/5, 13, [weapon(sword, 3, 7)], []),
	Unit2 = unit(2, red, dog, 1/2, 1/1, 0/1, 12, [weapon(bite, 1, 3)], []),
	Unit3 = unit(3, blue, guy, 5/5, 10/10, 0/5, 15, [weapon(sword, 3, 7)], []),
	Unit4 = unit(4, blue, cat, 5/4, 1/1, 0/1, 12, [weapon(sword, 1, 3)], []),
	State0 = [0-Unit1, 0-Unit2, 0-Unit3, 0-Unit4],
	do(next_turn, State0, State, Cues).

run([], State, State, []).
run([G|Goals], State0, State, [Cues|Cs]) :-
	call(do, G, State0, State1, Cues),
	run(Goals, State1, State, Cs).

do(Action, State0, State1, Cues) :-
	format("Action: ~w~n", [Action]),
	effect(Action, State0, State1, Cues),
	% call(Goal, State0, State1, Cues),
	dump_state(State1),
	format("Cues: ~w~n", [Cues]),
	menu(State1, Menu),
	format("Menu: ~w~n~n", [Menu]).

dump_state(State) :-
	maplist(dump_unit, State),
	current_turn(State, Who),
	format("Turn: ~w~n", [Who]).

dump_unit(CT-Unit) :-
	format("CT(~w): ~w~n", [CT, Unit]).
