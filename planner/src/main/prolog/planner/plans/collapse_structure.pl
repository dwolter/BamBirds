:- module(plans_collapse_structure, [collapseStructure/5, plans_common:plan/2]).
:- use_module(common).
:- use_module(planner(ab)).
:- use_module(planner(geometric)).
:- use_module(planner(physics/impact)).
:- use_module(planner(shot)).
:- use_module(planner(data)).

% This strategy aims to collapse a detected structure

plans_common:plan(Bird,plan{bird:Bird, shot:Shot, target_object:Target, impact_angle:ImpactAngle, strategy:"collapseStructure", confidence:C, reasons:Pigs}) :-
	collapseStructure(Bird, Target, ImpactAngle, C, Pigs),
	shot_params_dict(ImpactAngle, Shot).

/*
plan(Bird,[Target, Angle, "collapseStructure", C, Pigs]) :-
	collapseStructure(Bird, CTarget, CAngle, Conf, Pigs),
	((max_damage(Bird, CTarget, CAngle, MT, A, EXTRA), length(EXTRA, X))
	-> (Target=MT, Angle=A, C is min(1.0, Conf+0.05*X));
	 (Target=CTarget, Angle=CAngle, C is Conf)).
*/

max_supporter([], [], 0).
max_supporter([Cand|CS], MS, MScore) :-
	\+isHittable(Cand,_),
	max_supporter(CS,MS,MScore).
% Variant if supports predicate is not present
max_supporter([Cand|CS], MS, MScore) :-
	isHittable(Cand,_),
	\+ supports(_,_),
	max_supporter(CS, MS2, MScore2),
	aggregate_all(count, isOn(_,Cand), Above), !,
	(Above > MScore2 -> (MS=[Cand], MScore is Above) ;
		(Above==MScore2 -> (MS=[Cand|MS2], MScore is MScore2) ;
		(MS=MS2, MScore is MScore2))).
% Variant if supports predicate is present
max_supporter([Cand|CS], MS, MScore) :-
	isHittable(Cand,_),
	supports(_,_), !,
	max_supporter(CS, MS2, MScore2),
	aggregate_all(count, supports(Cand,_), CountSupported), !,
	(CountSupported > MScore2 -> (MS=[Cand], MScore is CountSupported) ;
		(CountSupported==MScore2 -> (MS=[Cand|MS2], MScore is MScore2) ;
		(MS=MS2, MScore is MScore2))).

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
	member(O, Set1),
	member(O, Set2), !.

objects_protect_pigs(Objs, Pigs) :-
	findall(P, (pig(P), member(P, Objs)), Pigs),
	Pigs \= [], !.

objects_protect_pigs(Objs, Pigs) :-
	findall(P, (pig(P), shot_obstacles(P, OS, _), sets_overlap(OS,Objs), !), Pigs),
	Pigs \=[], !.

collapseTarget(Bird, Critical, Critical) :-
	hasColor(Bird,black) ; (impact(Bird, Critical, I), I>=1.0) ; loose(Critical).

collapseTarget(Bird, Critical, Target) :-
	isBelow(Object, Critical),  % add stability check
	collapseTarget(Bird, Object, Target).

collapseStructure(Bird, Target, Angle, C, Pigs) :-
	\+(hasColor(Bird,black)),
	structure(Structure),
	pigs_in_struct(Structure, N, Pigs),
	N>0,
	belongsTo(Target, Structure),
	isHittable(Target, _),
	hasOrientation(Target, horizontal),
	\+isLeft(_,Target),
	\+((isOn(Target,X), \+(hill(X) ; hasOrientation(X,vertical)))),
	isOn(Target, XX),
	\+hill(XX),
	\+(lean_to_hill(Target)),
	C is 0.9,
	flachschuss(Target, Angle).

% collapseStructure_(+Birds, -Target, -Confidence)
collapseStructure(Bird, Target, Angle, C, Pigs) :-
	belongsTo(Target, Structure),
%    pigs_in_struct(Structure, N, Pigs),
%    N > 0, % FIXME: besser wahrscheinlichkeit des erlegens der schweine
	findall(O, belongsTo(O,Structure), Objs),
	%    bounding_box_lst(Objs, X0, Y0, X1, Y1),
	objects_protect_pigs(Objs, Pigs),
	max_supporter(Objs, Criticals, S),
	critical_effect(Bird, Target, Criticals, Modifier),
%    member(Target, Criticals),
%    collapseTarget(Bird, Critical, Target),
	findall(A, isHittable(Target,A), AS),
	max_list(AS, Angle),
	\+pig(Target),
	(\+hasMaterial(Target,stone,_,_,_,_) ; hasColor(Bird,black)),
	(((hasMaterial(Target,wood),hasColor(Bird,yellow)) ; (hasMaterial(Target,ice),hasColor(Bird,blue))) -> Bonus is 0.2 ; Bonus is 0.0),
	length(Objs, Num_Of_Objs),
	impact(Bird, Target, I),
	%format('S==~w, Num_Of_Objs=~w, I=~w, Target=~w~n', [S,Num_Of_Objs,I,Target]),
	C is min(1.0,0.6 + 0.1*((S+1)/ Num_Of_Objs) + Bonus + 0.00002*I + Modifier).

% nudge whole structure that rests on a ball
collapseStructure(_, Target, Angle, 0.8, Pigs) :-
	structure(Structure),
	pigs_in_struct(Structure, N, Pigs),
	N>0,
	belongsTo(B,Structure),
	hasForm(B,ball),
	isOn(B,ground),
	isOn(Target, B),
	isHittable(Target, _),
	flachschuss(Target, Angle),
	Angle > -45.