:- module(plans_collapse_structure, [collapseStructure/5, plans_common:plan/2]).
:- use_module(common).
:- use_module(planner(ab)).
:- use_module(planner(geometric)).
:- use_module(planner(physics/impact)).
:- use_module(planner(shot)).
:- use_module(planner(data)).
:- use_module(library(yall)).

% This strategy aims to collapse a detected structure

plans_common:plan(Bird,plan{bird:Bird, shot:Shot, target_object:Target, impact_angle:ImpactAngle, strategy:"collapseStructure", confidence:C, reasons:Pigs}) :-
	\+ whitebird(Bird),
	collapseStructure(Bird, Target, UUID, C, Pigs),
	shot_params_dict(UUID, Shot, ImpactAngle).

/*
plan(Bird,[Target, Angle, "collapseStructure", C, Pigs]) :-
	collapseStructure(Bird, CTarget, CAngle, Conf, Pigs),
	((max_damage(Bird, CTarget, CAngle, MT, A, EXTRA), length(EXTRA, X))
	-> (Target=MT, Angle=A, C is min(1.0, Conf+0.05*X));
	 (Target=CTarget, Angle=CAngle, C is Conf)).
*/

isOnRatio(Obj1,Obj2,Ratio) :-
	isOn(Obj1,Obj2),
	hasMaterial(Obj1,_,X1,_,Width1,_),
	hasMaterial(Obj2,_,X2,_,Width2,_),
	(X2 =< X1+Width1, X1 =< X2+Width2),
	Overlapping is min(X1+Width1, X2+Width2) - max(X1, X2),
	Ratio is Overlapping / Width1.

supportingFactor(Obj2,Obj1,Factor) :-
	isOnRatio(Obj1,Obj2,RelevantRatio),
	findall(Ratio,
		(
			isOnRatio(Obj1,OtherObj,Ratio),
			OtherObj \= Obj2
		),
		OtherRatios
	),
	sum_list(OtherRatios,SumOtherRatios),
	((RelevantRatio + SumOtherRatios > 0) ->
	Factor is RelevantRatio / (RelevantRatio + SumOtherRatios);
	Factor is 0).

supporting(Obj,Amount) :-
	findall(F,
		(
			supportingFactor(Obj,_,F)
		),
		Factors
	),
	sum_list(Factors,Amount).

independent_supports(Obj,SupportedObj) :-
	% \+ supports(_,_) -> 
		% Variant if supports predicate is not present
		% (
			isOn(SupportedObj,Obj),
			destroyable(Obj),
			destroyable(SupportedObj),
			supportingFactor(Obj,SupportedObj,Factor),
			Factor > 0.3.
		% );
		% Variant if supports predicate is present
		% supports(Obj,SupportedObj).

:- table all_supports/2.

all_supports(Obj,AllSupports) :-
	setof(SupportedObj, (
		independent_supports(Obj, SupportedObj)
	), AllSupports) ->
	true;
	AllSupports = [].

recursive_all_supports(Obj, Intermediate, AllSupports) :-
	all_supports(Obj, AllObjSupports),
	union(Intermediate, AllObjSupports, CurrentIntermediate),
	subtract(AllObjSupports, Intermediate, AllNewSupports),
	findall(RecursiveSupports,
		(
			member(NewSupport, AllNewSupports),
			recursive_all_supports(NewSupport, CurrentIntermediate, RecursiveSupports)
		),
		AllRecursiveSupports
	),
	flatten(AllRecursiveSupports, FlattenedRecursiveSupports),
	sort(FlattenedRecursiveSupports, FlattenedRecursiveSupportsSet),
	union(AllNewSupports, FlattenedRecursiveSupportsSet, AllSupports).

recursive_all_supports(Obj, AllSupports) :-
	recursive_all_supports(Obj, [], AllSupports).

count_supports(Obj,Count) :-
	aggregate(count, independent_supports(Obj,_), Count).

recursive_count_supports(Obj,Count) :-
	recursive_all_supports(Obj,AllSupports),
	length(AllSupports,Count).

joint_supports([], [], 0).
joint_supports([Obj|Objs], Supports, Factor) :-
	joint_supports(Objs, DeepSupports, DeepFactor),
	recursive_all_supports(Obj, CurrentSupports),
	union(DeepSupports,CurrentSupports,UnfilteredSupports),
	subtract(UnfilteredSupports,[Obj|Objs],Supports),
	supporting(Obj,CurrentFactor),
	Factor is DeepFactor + CurrentFactor.

max_supporter([], [], 0).
max_supporter([Cand|CS], MS, MScore) :-
	\+isHittable(Cand,_),
	max_supporter(CS,MS,MScore).
max_supporter([Cand|CS], MS, MScore) :-
	once(isHittable(Cand,_)),
	max_supporter(CS, MS2, MScore2),
	recursive_count_supports(Cand, CountSupported), !,
	(CountSupported > MScore2 -> (MS=[Cand], MScore is CountSupported) ;
		(CountSupported==MScore2 -> (MS=[Cand|MS2], MScore is MScore2) ;
		(MS=MS2, MScore is MScore2))).

:- begin_tests(structure_max_supporter).

test(structure_max_supporter_is_on_single, [setup((data:purge,abolish_all_tables))]) :-
	% Given
	data:assertz(isHittable(objA,"uid")),
	data:assertz(isOn(objB,objA)),
	data:assertz(hasMaterial(objA, wood,584,300,41,29)),
	data:assertz(hasMaterial(objB, wood,584,300,41,29)),
	% When
	max_supporter([objA,objB],[MaxSupporter],MScore),
	% Then
	assertion(MaxSupporter = objA),
	assertion(MScore = 1).

test(structure_max_supporter_supports_single, [blocked('supports/2 is disabled'),setup((data:purge,abolish_all_tables))]) :-
	% Given
	data:assertz(isHittable(objA,"uid")),
	data:assertz(supports(objA,objB)),
	% When
	max_supporter([objA,objB],[MaxSupporter],MScore),
	% Then
	assertion(MaxSupporter = objA),
	assertion(MScore = 1).
test(structure_max_supporter_supports_multiple_layers, [blocked('supports/2 is disabled'),setup((data:purge,abolish_all_tables))]) :-
	% Given
	data:assertz(isHittable(objA,"uid")),
	data:assertz(isHittable(objC,"uid")),
	data:assertz(supports(objA,objB)),
	data:assertz(supports(objC,objA)),
	% When
	max_supporter([objA,objB,objC],[MaxSupporter],MScore),
	% Then
	assertion(MaxSupporter = objC),
	assertion(MScore = 2).

test(structure_max_supporter_supports_multiple_layers_with_cycle, [blocked('supports/2 is disabled'),setup((data:purge,abolish_all_tables))]) :-
	% Given
	data:assertz(isHittable(objA,"uid")),
	data:assertz(isHittable(objC,"uid")),
	data:assertz(supports(objA,objB)),
	data:assertz(supports(objC,objA)),
	data:assertz(supports(objB,objC)),
	% When
	max_supporter([objA,objB,objC],MaxSupporter,MScore),
	% Then (only a and c are hittable)
	assertion(subtract([objA,objC],MaxSupporter,[])),
	assertion(MScore = 3).

:- end_tests(structure_max_supporter).

%max_supporter([Cand|CS], MS, MScore) :-
%    max_supporter(CS, MS2, MScore2),
%    (isHittable(Cand,_) -> (findall(U, isOn(U,Cand), US),
%			    length(US, Cnt),
%			    (Cnt >= MScore2 -> (MScore is Cnt, MS=Cand) ; (MScore is MScore2, MS = MS2)))
%    ; (MScore is MScore2, MS=MS2)).

sum_width([],X,X).
sum_width([O|OS], Accu, S) :-
	hasMaterial(O,_,_,_,W,_),
	Accu2 is Accu + W,
	sum_width(OS,Accu2,S).

loose(Object) :-
	hasMaterial(Object, _, _, _, Width, _),
	findall(O, isOn(O,Object), Above),
	sum_width(Above, 0, AW),
	AW < Width div 4,
	findall(O, isOn(Object,O), Below),
	sum_width(Below, 0, BW),
	BW < Width div 4.

critical_effect(_,Target, Criticals, 0.0) :-
	member(Target, Criticals).
critical_effect(Bird,Target, Criticals, -0.2) :-
	member(X, Criticals),
%    aggregate_all(count, isOn(X,_), Supps),
	isOn(X, Target),
	penetration(Bird, [Target], E),
	E < 0.9.

% checks whether a target connects vis isLeft/isRight to a hill
lean_to_hill(Target) :-
	isLeft(Target, X),
	lean_to_hill(X).

lean_to_hill(Target) :-
	\+(isLeft(Target, _)),
	hasMaterial(Target, _, X, Y, W, H),
	PX0 is (X+W-1),
	PX1 is (PX0+6),
	PY is (Y+0.5*H),
	hill(Hill),
	shape(Hill, poly, _, _, _, [ _ | POINTS]),
	line_poly_intersection(PX0,PY,PX1,PY, POINTS, _, _).

sets_overlap(Set1, Set2) :-
	\+ intersection(Set1, Set2, []).

objects_protect_pigs(Objs, Pigs, CumulativeAmountDestroyed) :-
	findall((P,Amount), (
			pig(P),
			fewest_shot_obstacles_no_hill(P, OS),
			intersection(OS,Objs, ProtectingObjsAffected),
			length(OS,ObstacleLength),
			length(ProtectingObjsAffected,AffectedLength),
			AffectedLength > 0,
			Amount is AffectedLength / ObstacleLength
		),
		PigsAndAmount
	),
	(PigsAndAmount = [] -> 
		(Pigs = [], CumulativeAmountDestroyed = 0);
		(
			maplist([(P,_), P]>>true, PigsAndAmount, Pigs),
			aggregate_all(sum(A), member((_,A), PigsAndAmount),CumulativeAmountDestroyed)
		)
	).

average_distance(_,[],0) :- !.
average_distance(Obj,Objs,AverageDistance) :-
	shape(Obj,_, PX,PY,_,_),
	findall(Dist,(
			member(O,Objs),
			shape(O,_, CX,CY,_,_),
			distance([PX,PY],[CX,CY],Dist)
		),
		Distances),
	sum_list(Distances,Sum),
	length(Distances,Count),
	AverageDistance is Sum / Count.

collapseTarget(Bird, Critical, Critical) :-
	hasColor(Bird,black) ; (impact(Bird, Critical, I), I>=1.0) ; loose(Critical).

collapseTarget(Bird, Critical, Target) :-
	isBelow(Object, Critical),  % add stability check
	collapseTarget(Bird, Object, Target).

% Kind of random strategy with too high confidence..
% Target an object that
% - does not have anything left of it
% - in a structure that has at least one pig in it
% - is horizontal
% - is on another object that is neither a hill nor vertically aligned
% - does not lean to a hill
% FIXME: Disabled because lean_to_hill has infinite recursion
% collapseStructure(Bird, Target, UUID, C, Reasons) :-
% 	\+(hasColor(Bird,black)),
% 	structure(Structure),
% 	pigs_in_struct(Structure, N, Pigs),
% 	N>0,
% 	belongsTo(Target, Structure),
% 	isHittable(Target, _),
% 	hasOrientation(Target, horizontal),
% 	\+isLeft(_,Target),
% 	\+((isOn(Target,X), \+(hill(X) ; hasOrientation(X,vertical)))),
% 	\+(lean_to_hill(Target)),
% 	C is 0.6,
% 	flachschuss(Target, UUID),
% 	merge_reasons([], Pigs, [], Reasons).

% collapseStructure_(+Birds, -Target, -Confidence)
% Complex version for collapsing structures:
% - Find shots that destroy as many supporters as possible
% - Return supported and directly destroyed pigs as reasons
% - Decrease confidence if pigs in struct may not be affected
collapseStructure(Bird, Target, UUID, C, Reasons) :-
	belongsTo(Target, Structure),
	isHittable(Target, UUID),
	% Find objects that are destroyed by shot
	destroyed_objects(Bird, Target, UUID, DestroyedSupporters),
	% Find all objects that these destroyed objects support and their factor of supporting
	joint_supports(DestroyedSupporters, AllSupports, JointSupportingFactor),
	% There must be at least one object that is supported
	length(AllSupports, S),
	S > 0,
	% Join all directly affected pigs together
	include(pig,AllSupports,SupportedPigs),
	include(pig,DestroyedSupporters,DestroyedPigs),
	union(DestroyedPigs,SupportedPigs,Pigs),
	% Possible protected pigs
	objects_protect_pigs(AllSupports, ProtectedPigs,CumulativeObstaclesDestroyed),
	subtract(ProtectedPigs, Pigs, OnlyProtectedPigs),
	% Possibly affected pigs
	pigs_in_struct(Structure, _, PigsInStruct),
	union(OnlyProtectedPigs, Pigs, ProtectedAndFreedPigs),
	subtract(PigsInStruct, ProtectedAndFreedPigs, PossiblyAffectedPigs),
	average_distance(Target,PossiblyAffectedPigs,AverageDistanceAffected),
	% Calculate the confidence
	height(Target,H),
	C is min(1,0.6 + 0.05*JointSupportingFactor*S + 0.01*H + 0.1*CumulativeObstaclesDestroyed - 0.005*AverageDistanceAffected),
	merge_reasons(Pigs, PossiblyAffectedPigs, OnlyProtectedPigs, Reasons),
	Reasons \= [].

% nudge whole structure that rests on a ball
collapseStructure(_, Target, UUID, 0.7, Reasons) :-
	structure(Structure),
	pigs_in_struct(Structure, N, Pigs),
	N>0,
	belongsTo(B,Structure),
	hasForm(B,ball),
	isOn(B,ground),
	isOn(Target, B),
	isHittable(Target, _),
	flachschuss(Target, UUID),
	parabola(Target, UUID, _, Angle, _,_),
	Angle > -45,
	merge_reasons([], Pigs, [], Reasons).