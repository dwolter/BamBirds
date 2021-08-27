:- module(plans_common, [
	is_goal/2, 
	proper_goal/3, 
	worth/1,
	pig_score/5,
	pigs_bomb_score/5,
	pigs_in_range/4,
	plan/2,
	plan_last_resort/2,
	plan_last_resort/3
	]).
:- use_module(planner(ab)).
:- use_module(planner(data)).
:- multifile plan/2.
:- multifile plan_last_resort/2.
:- multifile plan_last_resort/3.

% identify objects that are worth shooting at
is_goal(Object, 1.0) :-
	pig(Object).

is_goal(Object, 0.8) :-
	hasMaterial(Object, tnt, _, _, _, _).

is_goal(Object, 0.6) :-
	hasForm(Object,ball).

proper_goal(Pig, 1.0, [Pig]) :-
	pig(Pig).

proper_goal(TNT, C, Pigs) :-
	hasMaterial(TNT,tnt,_,_,_,_),
	findall(P, (pig(P), canExplode(TNT, P)), Pigs),
	belongsTo(TNT, Struct),
	findall(O, belongsTo(O, Struct), Damaged),
	aggregate_all(count, (member(P, Damaged), pig(P)), PigsDamaged),
	length(Damaged, NObjects),
	%	format('target=~w, Pigs=~w, PigsDamaged=~w, objects damaged=~w~n', [TNT,Pigs,PigsDamaged,NObjects]),
	((Pigs=[], PigsDamaged==0) -> 
		C is 0.491;
		(Pigs\=[] -> 
			C is 1.0;
			C is min(0.9, 0.5+0.05*NObjects)
		)
	).

pig_score(X, Y, Pig, Killed, S) :-
	shape(Pig, _, CX, CY, _, _),
	D is ((X-CX)^2 + (Y-CY)^2),
	(D<1500 -> Killed=[Pig] ; Killed=[]),
%    writeln(D),
	S is max(0.0, 1.0 - 0.004*sqrt(D)).

pigs_bomb_score(_, _, [], [], 0).

pigs_bomb_score(X,Y, [P|Ps], Killed, S) :-
	pigs_bomb_score(X, Y, Ps, KPs, S1),
	pig_score(X, Y, P, KP, S2),
	append(KP, KPs, Killed),
	S is S1 + S2.

pigs_in_range(X, Y, Score, KilledPigs) :-
	findall(P, pig(P), AllPigs),
	pigs_bomb_score(X, Y, AllPigs, KilledPigs, S),
	(KilledPigs=[] -> Score is 0 ; Score is min(1.0,S)).

worth(Struct) :-
	belongsTo(X, Struct),
	is_goal(X,_), !.
