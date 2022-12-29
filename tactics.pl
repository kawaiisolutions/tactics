% :- module(tactics, [test/0]).

unit_id(unit(ID, _, _, _, _, _, _, _), ID).
unit_team(unit(_, Team, _, _, _, _, _, _), Team).
unit_type(unit(_, _, Type, _, _, _, _, _), Type).
unit_pos(unit(_, _, _, Pos, _, _, _, _), Pos).
unit_health(unit(_, _, _, _, Health, _, _, _), Health).
unit_hp(unit(_, _, _, _, HP/_, _, _, _), HP).
unit_maxhp(unit(_, _, _, _, _/MaxHP, _, _), MaxHP).
unit_mana(unit(_, _, _, _, _, Mana, _, _), Mana).
unit_mp(unit(_, _, _, _, _, MP/_, _, _), MP).
unit_maxmp(unit(_, _, _, _, _, _/MaxMP, _, _), MaxMP).
unit_speed(unit(_, _, _, _, _, _, Speed, _), Speed).
unit_status(unit(_, _, _, _, _, _, _, Status), Status).

unit_with_pos(To,
	unit(ID, Team, Type, _Pos, Health, Mana, Speed, Status),
	unit(ID, Team, Type, To, Health, Mana, Speed, Status)).

unit_with_status(+X,
	unit(ID, Team, Type, Pos, Health, Mana, Speed, Status),
	unit(ID, Team, Type, Pos, Health, Mana, Speed, [X|Status])).
unit_with_status(-X,
	unit(ID, Team, Type, Pos, Health, Mana, Speed, Status0),
	unit(ID, Team, Type, Pos, Health, Mana, Speed, Status)) :-
	once(subtract(Status0, [X], Status)).

unit_with_hp(+X,
	unit(ID, Team, Type, Pos, HP0/MaxHP, Mana, Speed, Status),
	unit(ID, Team, Type, Pos, HP/MaxHP, Mana, Speed, Status)) :-
	HP is min(HP0 + X, MaxHP).
unit_with_hp(-X,
	unit(ID, Team, Type, Pos, HP0/MaxHP, Mana, Speed, Status),
	unit(ID, Team, Type, Pos, HP/MaxHP, Mana, Speed, Status)) :-
	HP is max(HP0 - X, 0).
unit_with_mp(+X,
	unit(ID, Team, Type, Pos, Health, MP0/MaxMP, Speed, Status),
	unit(ID, Team, Type, Pos, Health, MP/MaxMP, Speed, Status)) :-
	MP is min(MP0 + X, MaxMP).
unit_with_mp(-X,
	unit(ID, Team, Type, Pos, Health, MP0/MaxMP, Speed, Status),
	unit(ID, Team, Type, Pos, Health, MP/MaxMP, Speed, Status)) :-
	MP is max(MP0 - X, 0).


unit_has_status(X, Unit) :-
	unit_status(Unit, Status),
	memberchk(X, Status).

pos(X/Y) :-
	% TODO: based on map size
	between(1, 12, X),
	between(1, 12, Y).

%% effect(+Effect, +BeforeState, -AfterState, -Cues).

effect(tick, CT0-Unit, CT-Unit, []) :-
	unit_speed(Unit, Speed),
	CT is CT0 + Speed.

effect(begin_turn, CT-Unit0, CT-Unit, [focus_unit(ID)]) :-
	unit_with_status(-wait, Unit0, Unit1),
	unit_with_status(-attacked, Unit1, Unit2),
	unit_with_status(-moved, Unit2, Unit3),
	unit_with_mp(+1, Unit3, Unit),
	unit_id(Unit, ID).

effect(move(To), CT0-Unit0, CT-Unit, [move_unit(ID, To)]) :-
	pos(To),
	can_move(Unit0, To),
	unit_with_pos(To, Unit0, Unit1),
	unit_with_status(+moved, Unit1, Unit),
	unit_id(Unit, ID),
	ct_cost(move, Cost),
	CT is CT0 - Cost.

effect(end_turn, CT0-Unit0, CT-Unit, []) :-
	\+unit_has_status(wait, Unit0),
	unit_with_status(+wait, Unit0, Unit),
	ct_cost(end_turn, Cost),
	CT is CT0 - Cost.

ct_cost(move, 20).
ct_cost(attack, 20).
ct_cost(end_turn, 60).

can_do(move, State) :-
	current_unit(State, Unit),
	\+unit_has_status(wait, Unit),
	\+unit_has_status(moved, Unit),
	move_range(Unit, Range),
	Range > 0.
can_do(attack, State) :-
	current_unit(State, Unit),
	\+unit_has_status(wait, Unit),
	\+unit_has_status(attacked, Unit).
can_do(end_turn, State) :-
	current_unit(State, Unit),
	\+unit_has_status(wait, Unit).

menu(State, Actions) :-
	findall(Action, can_do(Action, State), Actions).

next_turn(State0, [Unit|State], Cues) :-
	next_turn_(State0, [Unit0|State], Cues0),
	effect(begin_turn, Unit0, Unit, Cues1),
	append(Cues0, Cues1, Cues).
next_turn_(State0, State, Cues) :-
	tick(State0, State1, Cues0),
	[CT-_|_] = State1,
	(  CT >= 100
	-> State = State1, Cues = Cues0
	;  next_turn_(State1, State, Cues1), append(Cues0, Cues1, Cues)
	).

tick(State0, State, [tick|Cues]) :-
	maplist(effect(tick), State0, State1, Cs),
	keysort(State1, State2),
	reverse(State2, State),
	flatten(Cs, Cues).

end_turn([Unit0|State0], [Unit|State0], Cues) :-
	effect(end_turn, Unit0, Unit, Cues).

move(To, [Unit0|State], [Unit|State], Cues) :-
	freeze(Blocker, unit_pos(Blocker, To)),
	\+memberchk(_-Blocker, State),
	effect(move(To), Unit0, Unit, Cues).

attack(Target, [CT0-Unit0|State0], [CT-Unit|State], [attack(ID, Target), damage(Target, Damage)]) :-
	\+unit_has_status(wait, Unit0),
	\+unit_has_status(attacked, Unit0),
	unit_id(Unit, ID),
	select_unit(Target, VCT-Victim0, State0, State1),
	Damage is 4, % test
	ct_cost(attack, Cost),
	CT is CT0 - Cost,
	unit_with_status(+attacked, Unit0, Unit),
	unit_with_hp(-Damage, Victim0, Victim),
	sort_state([VCT-Victim|State1], State).

current_turn(State, Team) :-
	current_unit(State, Unit),
	unit_team(Unit, Team).
current_unit([_-Unit|_], Unit).

select_unit(ID, CT-Unit, State0, State) :-
	freeze(Unit, unit_id(Unit, ID)),
	once(select(CT-Unit, State0, State)).

sort_state(State0, State) :-
	keysort(State0, State1),
	reverse(State1, State).

can_move(Unit, To) :-
	\+unit_has_status(moved, Unit),
	unit_pos(Unit, From),
	move_range(Unit, Range),
	distance(From, To, Dist),
	Dist =< Range.

move_range(Unit, Range) :-
	unit_type(Unit, Type),
	unit_type_move_range(Type, Range).

% can_attack(A, B)

distance(X0/Y0, X1/Y1, Dist) :-
	Dist is abs(X0-X1) + abs(Y0-Y1).

unit_type_move_range(soldier, 4).
unit_type_move_range(guy, 3).

test :-
	srandom(42),
	Unit1 = unit(1, red, soldier, 1/1, 10/10, 5/5, 15, []),
	Unit2 = unit(2, blue, guy, 5/5, 10/10, 0/5, 25, []),
	State = [0-Unit1, 0-Unit2],
	run([next_turn, move(4/5), end_turn,
		next_turn, end_turn,
		next_turn, attack(1), move(3/4), end_turn,
		next_turn, move(2/2), end_turn,
		next_turn], State, _, Cues),
	write(Cues),
	!.

begin(Seed, State, Cues) :-
	srandom(Seed),
	Unit1 = unit(1, red, soldier, 1/1, 10/10, 5/5, 13, []),
	Unit2 = unit(2, blue, guy, 5/5, 10/10, 0/5, 15, []),
	State0 = [0-Unit1, 0-Unit2],
	do(next_turn, State0, State, Cues).

run([], State, State, []).
run([G|Goals], State0, State, [Cues|Cs]) :-
	call(do, G, State0, State1, Cues),
	run(Goals, State1, State, Cs).

do(Goal, State0, State1, Cues) :-
	format("Action: ~w~n", [Goal]),
	call(Goal, State0, State1, Cues),
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
