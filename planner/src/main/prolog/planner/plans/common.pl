:- module(plans_common, [
	is_goal/2, 
	proper_goal/3, 
	worth/1,
	pig_bomb_confidence/5,
	pigs_bomb_confidence/5,
	pigs_in_range/4,
	plan/2,
	plan_last_resort/2,
	plan_last_resort/3,
	merge_reasons/4,
	reasons_set/2,
	extract_all_targets_from_reasons/2,
	extract_targets_from_reasons/4
	]).
:- use_module(planner(ab)).
:- use_module(planner(data)).
:- use_module(library(yall)).
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

proper_goal(Pig, 1.0, [destroy(Pig)]) :-
	pig(Pig).

proper_goal(TNT, C, Reasons) :-
	hasMaterial(TNT,tnt,_,_,_,_),
	findall(P, (pig(P), canExplode(TNT, P)), Pigs),
	belongsTo(TNT, Struct),
	findall(O, belongsTo(O, Struct), Damaged),
	include(pig, Damaged, AllPigsDamaged),
	subtract(AllPigsDamaged, Pigs, PigsDamaged),
	length(PigsDamaged, NPigsDamaged),
	length(Damaged, NObjects),
	%	format('target=~w, Pigs=~w, PigsDamaged=~w, objects damaged=~w~n', [TNT,Pigs,PigsDamaged,NObjects]),
	((Pigs=[], NPigsDamaged==0) -> 
		(C is 1, merge_reasons([TNT], [], [], Reasons));
		(Pigs\=[] -> 
			(
				C is 0.9,
				merge_reasons([TNT|Pigs], PigsDamaged, [], Reasons)
			);
			(
				C is min(0.9, 0.5+0.05*NObjects),
				merge_reasons([TNT], PigsDamaged, [], Reasons)
			)
		)
	).

pig_bomb_confidence(X, Y, Pig, Killed, C) :-
	shape(Pig, _, CX, CY, _, _),
	Distance is ((X-CX)^2 + (Y-CY)^2),
	DistanceFromSafeExplode is max(0,Distance - 3600),
	C is max(0.0, 1.0 - 0.02*sqrt(DistanceFromSafeExplode)),
	(C>0 -> Killed=[Pig] ; Killed=[]).
%    writeln(D),.

pigs_bomb_confidence(_, _, [], [], 0).
pigs_bomb_confidence(X,Y, [P|Ps], Killed, C) :-
	pigs_bomb_confidence(X, Y, Ps, KPs, C1),
	pig_bomb_confidence(X, Y, P, KP, C2),
	append(KP, KPs, Killed),
	C is C1 + C2.

pigs_in_range(X, Y, Confidence, KilledPigs) :-
	findall(P, pig(P), AllPigs),
	pigs_bomb_confidence(X, Y, AllPigs, KilledPigs, SumConfidences),
	length(KilledPigs, LKilledPigs),
	(LKilledPigs=0 -> 
		Confidence is 0;
		Confidence is SumConfidences / LKilledPigs
	).

worth(Struct) :-
	belongsTo(X, Struct),
	is_goal(X,_), !.

merge_reasons(Destroy, Affect, Free, Reasons) :-
	maplist([A,B]>>(B=destroy(A)), Destroy, DestroyMapped),
	maplist([C,D]>>(D=affect(C)), Affect, AffectMapped),
	maplist([E,F]>>(F=free(E)), Free, FreeMapped),
	append([DestroyMapped,AffectMapped,FreeMapped],Reasons).

extract_targets_from_reasons(Reasons, Destroy, Affect, Free) :-
	include([destroy(_)]>>true, Reasons, DestroyMapped),
	include([affect(_)]>>true, Reasons, AffectMapped),
	include([free(_)]>>true, Reasons, FreeMapped),
	maplist([destroy(A),A]>>true, DestroyMapped, Destroy),
	maplist([affect(B),B]>>true, AffectMapped, Affect),
	maplist([free(C),C]>>true, FreeMapped, Free).

extract_all_targets_from_reasons(Reasons,Targets) :-
	extract_targets_from_reasons(Reasons, Destroy, Affect, Free),
	append([Destroy, Affect, Free], Targets).
	

subtract_multiple(List,Delete1,Delete2,Result) :-
	subtract(List, Delete1, IntermediateResult),
	subtract(IntermediateResult, Delete2, Result).

reasons_set(DuplicateReasons, Reasons) :-
	extract_targets_from_reasons(DuplicateReasons, Destroy, Affect, Free),
	sort(Destroy,DestroyResult),
	sort(Affect,AffectSet),
	sort(Free,FreeSet),
	subtract_multiple(FreeSet,AffectSet,DestroyResult,FreeResult),
	subtract(AffectSet,DestroyResult,AffectResult),
	merge_reasons(DestroyResult, AffectResult, FreeResult, Reasons).