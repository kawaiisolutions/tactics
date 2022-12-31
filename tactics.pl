% unit(ID, Team, Type, Pos, Health, Mana, Speed, Status).

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
	unit_with_status(-acted, Unit1, Unit2),
	unit_with_status(-moved, Unit2, Unit3),
	unit_with_mp(+1, Unit3, Unit),
	unit_id(Unit, ID).

effect(move(To), CT0-Unit0, CT-Unit, [move_unit(ID, To)]) :-
	pos(To),
	can_move([0-Unit0], Unit0, To), % TODO
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
	\+ \+can_move(State, Unit, _).
can_do(attack, State) :-
	current_unit(State, Unit),
	\+ \+can_attack(Unit, _).
can_do(end_turn, State) :-
	current_unit(State, Unit),
	\+unit_has_status(wait, Unit).

menu(State, Actions) :-
	findall(Action, can_do(Action, State), Actions).

should_pass(State) :-
	menu(State, [end_turn]).

next_turn(State0, State, Cues) :-
	next_turn_(State0, State, Cues),
	\+should_pass(State).
next_turn(State0, State, Cues) :-
	next_turn_(State0, State1, Cues1),
	should_pass(State1),
	end_turn(State1, State2, Cues2),
	next_turn(State2, State, Cues3),
	append(Cues1, Cues2, CuesA),
	append(CuesA, Cues3, Cues).

next_turn_(State0, [Unit|State], Cues) :-
	next_turn_tick_(State0, [Unit0|State], Cues0),
	effect(begin_turn, Unit0, Unit, Cues1),
	append(Cues0, Cues1, Cues).
next_turn_tick_(State0, State, Cues) :-
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
	\+unit_at(State, To, _),
	effect(move(To), Unit0, Unit, Cues).

attack(Target, [CT0-Unit0|State0], [CT-Unit|State], [attack(ID, Target), damage(Target, Damage)]) :-
	unit_id(Unit, ID),
	select_unit(Target, VCT-Victim0, State0, State1),
	unit_pos(Victim0, TargetPos),
	can_attack(Unit0, TargetPos),
	attack_damage(Unit0, Victim0, Damage),
	ct_cost(attack, Cost),
	CT is CT0 - Cost,
	unit_with_status(+acted, Unit0, Unit),
	unit_with_hp(-Damage, Victim0, Victim1),
	unit_hp(Victim1, HP),
	(  HP =< 0
	-> unit_with_status(+dead, Victim1, Victim)
	;  Victim = Victim1	
	),
	sort_state([VCT-Victim|State1], State).

attack_damage(_Unit, _Victim, Damage) :-
	random_between(3, 7, Damage).

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

distance(X0/Y0, X1/Y1, Dist) :-
	Dist is abs(X0-X1) + abs(Y0-Y1).

can_move(State, Unit, To) :-
	\+unit_has_status(dead, Unit),
	\+unit_has_status(wait, Unit),
	\+unit_has_status(moved, Unit),
	unit_pos(Unit, From),
	move_range(Unit, Range),
	Range > 0,
	pos(To),
	\+unit_at(State, To, _),
	distance(From, To, Dist),
	Dist =< Range.

unit_at(State, Pos, Unit) :-
	freeze(Unit, unit_pos(Unit, Pos)),
	memberchk(_-Unit, State).

move_radius(State, Unit, Positions) :-
	findall(Pos, can_move(State, Unit, Pos), Positions).

attack_range(Unit, Range) :-
	unit_type(Unit, Type),
	unit_type_attack_range(Type, Range).

can_attack(Unit, Pos) :-
	\+unit_has_status(dead, Unit),
	\+unit_has_status(acted, Unit),
	\+unit_has_status(wait, Unit),
	unit_pos(Unit, X/Y),
	% exclude self?
	dif(Pos, X/Y),
	% test: cross-shaped attack pattern
	attack_range(Unit, Range),
	aoe(Range, X/Y, Pos).

attack_radius(Unit, Positions) :-
	findall(Pos, can_attack(Unit, Pos), Positions).

%% aoe(+Shape, +Center, -Pos).
% constrain Pos to area of effect shapes
aoe(circle(Range), Center, Pos) :-
	pos(Pos),
	distance(Center, Pos, Dist),
	Dist =< Range.
aoe(cross(Range), CX/CY, X/Y) :-
	( CX = X ; CY = Y ),
	pos(X/Y),
	distance(CX/CY, X/Y, Dist),
	Dist =< Range.

unit_type_move_range(soldier, 4).
unit_type_move_range(guy, 3).
unit_type_move_range(wizard, 2).

unit_type_attack_range(soldier, cross(1)).
unit_type_attack_range(guy, cross(2)).
unit_type_attack_range(wizard, circle(5)).

move_range(Unit, Range) :-
	unit_type(Unit, Type),
	unit_type_move_range(Type, Range).

match_status(State, win(Team)) :-
	findall(T, (member(_-U, State), unit_team(U, T), \+unit_has_status(dead, U)), Teams0),
	sort(Teams0, [Team]),
	!.
match_status(State, input(Team)) :-
	current_unit(State, Unit),
	unit_team(Unit, Team),
	!.
match_status(_, draw).

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
