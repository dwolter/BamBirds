:- module(plans_target_pig, [targetPig/5, plans_common:plan/2]).
:- use_module(planner(ab/objects)).
:- use_module(planner(geometric)).
:- use_module(planner(shot)).
:- use_module(common).
:- use_module(planner(data)).


% Finds a plan for targetting a Pig, returns a list of a target, the strategy chosen, confidence
% plan_(-DecisionList)
plans_common:plan(Bird, plan{bird:Bird, shot:Shot, target_object:Target, impact_angle:ImpactAngle, strategy:"targetPig", confidence:C, reasons:Pigs}) :-
	\+ whitebird(Bird),
	targetPig(Bird, Target, UUID, C, Pigs),
	>(C, 0.49),
	shot_params_dict(UUID, Shot, ImpactAngle).

% 
linksschwein(P, [], P).
linksschwein(P, [P2|Pigs], Target) :-
	pig(P, PX, _, _, _),
	pig(P2,P2X,_,_,_),
	(PX<P2X -> linksschwein(P,Pigs,Target) ; linksschwein(P2,Pigs,Target)).

topschwein(P, [], P).
topschwein(P, [P2|Pigs], Target) :-
	pig(P, _, PY, _, _),
	pig(P2,_,P2Y,_,_),
	(PY>P2Y -> topschwein(P,Pigs,Target) ; topschwein(P2,Pigs,Target)).


% select pig from an all-hittable set of pigs by choosing shot
% with chance of side-effects on other pigs
preferred_shot(_, [P|Pigs], Target, UUID, C) :-
	bounding_box_lst(Pigs, XMin, YMin, XMax, YMax),
	( ((YMax - YMin) > 2*(XMax - XMin)) -> 
		(
			linksschwein(P,Pigs,Best), 
			flachschuss(Best,BestUUID)
		),
		(
			topschwein(P,Pigs,Best), 
			steilschuss(Best,BestUUID)
		)
	),
	!,
	member(Target, [P|Pigs]),
	(Target == Best -> (UUID is BestUUID, C is 1.0) ; (isHittable(Target, UUID), C is 0.9)).

second_chance(OBJS, Pig) :-
	member(O,OBJS),
	((pig(O), Pig = O) ; (findall(O2, isOn(O2,O), ABOVE),second_chance(ABOVE, Pig))),
	!.

% direct shot at hittable pigs
targetPig(Bird, Target, UUID, C, [destroy(Target)]) :-
	findall(P, pig(P), Pigs),
	all_hittable(Pigs),
	findall(B, bird(B), Birds),
	length(Pigs, Num_Pigs),
	length(Birds, Num_Birds),
	Num_Pigs =< Num_Birds,
	preferred_shot(Bird, Pigs, Target, UUID, C).

% direct shot at hittable pig
targetPig(_, Target, UUID, 1, [destroy(Target)]) :-
	pig(Target),  %leftmost_pig(Target),
	steilschuss(Target, UUID).

% penetration shot at pig
targetPig(Bird, Target, UUID, 1, Reasons) :-
	pig(Target),
%    \+isHittable(Target,_),
%    writeln(Objects),
	destroyed_objects(Bird, Target, UUID, Destroyed),
	include(pig, Destroyed, DestroyedPigs),
	member(Target, DestroyedPigs),
	merge_reasons(DestroyedPigs, [], [], Reasons).