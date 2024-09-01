:- module(unit, [unit_id/2, unit_team/2, unit_type/2, unit_pos/2, unit_health/2, unit_hp/2, unit_maxhp/2,
	unit_mana/2, unit_mp/2, unit_maxmp/2, unit_speed/2, unit_status/2,
	unit_json/2,
	unit_with_pos/3, unit_with_hp/3, unit_with_mp/3, unit_with_status/3,
	unit_equipment/2, unit_weapon/2, unit_has_status/3,
	move_range/2, attack_range/2, tick_unit/2]).

:- use_module(library(lists)).

% unit(ID, Team, Type, Pos, Health, Mana, Speed, Status).

unit_id(unit(ID, _, _, _, _, _, _, _, _), ID).
unit_team(unit(_, Team, _, _, _, _, _, _, _), Team).
unit_type(unit(_, _, Type, _, _, _, _, _, _), Type).
unit_pos(unit(_, _, _, Pos, _, _, _, _, _), Pos).
unit_health(unit(_, _, _, _, Health, _, _, _, _), Health).
unit_hp(unit(_, _, _, _, HP/_, _, _, _, _), HP).
unit_maxhp(unit(_, _, _, _, _/MaxHP, _, _, _), MaxHP).
unit_mana(unit(_, _, _, _, _, Mana, _, _, _), Mana).
unit_mp(unit(_, _, _, _, _, MP/_, _, _, _), MP).
unit_maxmp(unit(_, _, _, _, _, _/MaxMP, _, _, _), MaxMP).
unit_speed(unit(_, _, _, _, _, _, Speed, _, _), Speed).
unit_equipment(unit(_, _, _, _, _, _, _, Equipment, _), Equipment).
unit_status(unit(_, _, _, _, _, _, _, _, Status), Status).

unit_json(
	CT-unit(ID, Team0, Type0, X/Y, HP/MaxHP, MP/MaxMP, Speed, Equipment0, Status0),
	{
		"id": ID,
		"team": Team,
		"ct": CT,
		"type": Type,
		"pos": {"x": X, "y": Y},
		"hp": HP,
		"maxhp": MaxHP,
		"mp": MP,
		"maxmp": MaxMP,
		"speed": Speed,
		"equipment": Equipment,
		"status": Status
	}
) :-
	maplist(atom_chars, [Team0, Type0|Status0], [Team, Type|Status]),
	equipment_json(Equipment0, Equipment).

equipment_json([], []).
equipment_json(Equipment, JSON) :- maplist(equipment_json_, Equipment, JSON).
equipment_json_(weapon(Name0, Min, Max), {"type": "weapon", "name": Name}) :- atom_chars(Name0, Name).

unit_with_pos(To,
	unit(ID, Team, Type, _Pos, Health, Mana, Speed, Equipment, Status),
	unit(ID, Team, Type, To, Health, Mana, Speed, Equipment, Status)).

unit_with_status(+X,
	unit(ID, Team, Type, Pos, Health, Mana, Speed, Equipment, Status),
	unit(ID, Team, Type, Pos, Health, Mana, Speed, Equipment, [X|Status])).
unit_with_status(-X,
	unit(ID, Team, Type, Pos, Health, Mana, Speed, Equipment, Status0),
	unit(ID, Team, Type, Pos, Health, Mana, Speed, Equipment, Status)) :-
	once(subtract(Status0, [X], Status)).

unit_with_hp(+X,
	unit(ID, Team, Type, Pos, HP0/MaxHP, Mana, Speed, Equipment, Status),
	unit(ID, Team, Type, Pos, HP/MaxHP, Mana, Speed, Equipment, Status)) :-
	HP is min(HP0 + X, MaxHP).
unit_with_hp(-X,
	unit(ID, Team, Type, Pos, HP0/MaxHP, Mana, Speed, Equipment, Status),
	unit(ID, Team, Type, Pos, HP/MaxHP, Mana, Speed, Equipment, Status)) :-
	HP is max(HP0 - X, 0).
unit_with_mp(+X,
	unit(ID, Team, Type, Pos, Health, MP0/MaxMP, Speed, Equipment, Status),
	unit(ID, Team, Type, Pos, Health, MP/MaxMP, Speed, Equipment, Status)) :-
	MP is min(MP0 + X, MaxMP).
unit_with_mp(-X,
	unit(ID, Team, Type, Pos, Health, MP0/MaxMP, Speed, Equipment, Status),
	unit(ID, Team, Type, Pos, Health, MP/MaxMP, Speed, Equipment, Status)) :-
	MP is max(MP0 - X, 0).

unit_has_status(X, Unit) :-
	unit_status(Unit, Status),
	memberchk(X, Status).

unit_weapon(Unit, weapon(Name, Min, Max)) :-
	unit_equipment(Unit, Equipment),
	memberchk(weapon(Name, Min, Max), Equipment).
unit_weapon(Unit, weapon(fist, 1, 1)) :-
	unit_equipment(Unit, Equipment),
	\+ memberchk(weapon(Name, Min, Max), Equipment).

tick_unit(CT0-Unit, CT-Unit) :-
	unit_speed(Unit, Speed),
	CT is CT0 + Speed.

move_range(Unit, Range) :-
	unit_type(Unit, Type),
	unit_type_move_range(Type, Range).

unit_type_move_range(soldier, 4).
unit_type_move_range(guy, 3).
unit_type_move_range(wizard, 2).
unit_type_move_range(cat, 3).
unit_type_move_range(dog, 3).

attack_range(Unit, Range) :-
	unit_type(Unit, Type),
	unit_type_attack_range(Type, Range).

unit_type_attack_range(soldier, cross(1)).
unit_type_attack_range(guy, cross(2)).
unit_type_attack_range(wizard, circle(5)).
unit_type_attack_range(cat, cross(1)).
unit_type_attack_range(dog, cross(1)).