:- module(world, [pos/1, pos_adjacent/2, path/4, unit_can_bypass/2, can_move/3, move_radius/3,
	unit_at/3, distance/3, can_attack/2, can_attack_from/3, attack_radius/2, aoe/3, aoe_area/3]).

:- use_module(library(lists)).
:- use_module(library(dif)).
:- use_module(unit).

pos(X/Y) :-
	% TODO: based on map size
	between(1, 12, X),
	between(1, 12, Y).

pos_adjacent(X/Y0, X/Y1) :- succ(Y0, Y1).
pos_adjacent(X/Y0, X/Y1) :- succ(Y1, Y0).
pos_adjacent(X0/Y, X1/Y) :- succ(X0, X1).
pos_adjacent(X0/Y, X1/Y) :- succ(X1, X0).

path(State, Unit, Dest, Path) :-
	\+unit_at(State, Dest, _),
	unit_pos(Unit, Origin),
	move_range(Unit, Range),
	pos(Dest),
	distance(Origin, Dest, Dist),
	Dist =< Range,
	between(Dist, Range, Length),
	length(Path, Length),
	path_(State, Unit, Origin, Dest, Path),
	!.
path_(_, _, Origin, Origin, []).
path_(State, _, Origin, Dest, [Dest]) :-
	\+unit_at(State, Dest, _),
	pos_adjacent(Origin, Dest).
path_(State, Unit, Origin, Dest, [Pos|Rest]) :-
	\+pos_adjacent(Origin, Dest),
	pos_adjacent(Origin, Pos),
	(  unit_at(State, Pos, Blocker)
	-> once(unit_can_bypass(Unit, Blocker))
	;  true
	),
	path_(State, Unit, Pos, Dest, Rest).

unit_can_bypass(Unit, Other) :-
	unit_team(Unit, Team),
	unit_team(Other, Team).
unit_can_bypass(_, Other) :-
	unit_has_status(dead, Other).

can_move(State, Unit, To) :-
	\+unit_has_status(dead, Unit),
	\+unit_has_status(wait, Unit),
	\+unit_has_status(moved, Unit),
	pos(To),
	\+unit_at(State, To, _),
	path(State, Unit, To, _Path).
	% format("path to ~w: ~w~n", [To, Path]).

move_radius(State, Unit, Positions) :-
	findall(Pos, can_move(State, Unit, Pos), Positions).

unit_at(State, Pos, Unit) :-
	unit_pos(Unit, Pos),
	memberchk(_-Unit, State).

distance(X0/Y0, X1/Y1, Dist) :-
	Dist is abs(X0-X1) + abs(Y0-Y1).

can_attack(Unit, Pos) :-
	unit_pos(Unit, X/Y),
	can_attack_from(X/Y, Unit, Pos).

can_attack_from(X/Y, Unit, Pos) :-
	\+unit_has_status(dead, Unit),
	\+unit_has_status(acted, Unit),
	\+unit_has_status(wait, Unit),
	% exclude self?
	dif(Pos, X/Y),
	% test: cross-shaped attack pattern
	attack_range(Unit, Range),
	aoe(Range, X/Y, Pos).

attack_radius(Unit, Positions) :-
	findall(Pos, can_attack(Unit, Pos), Positions).

%% aoe(+Shape, +Origin, -Pos).
% constrain Pos to area of effect shapes
aoe(point, Pos, Pos) :-
	pos(Pos).
aoe(circle(Range), Origin, Pos) :-
	pos(Origin),
	pos(Pos),
	distance(Origin, Pos, Dist),
	Dist =< Range.
aoe(cross(Range), OriginX/OriginY, X/Y) :-
	pos(OriginX/OriginY),
	( OriginX = X ; OriginY = Y ),
	pos(X/Y),
	distance(OriginX/OriginY, X/Y, Dist),
	Dist =< Range.

aoe_area(AOE, Origin, Positions) :-
	findall(Pos, aoe(AOE, Origin, Pos), Positions).