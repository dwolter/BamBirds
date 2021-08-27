:- module(plans_target_pig, [targetPig/5, plans_common:plan/2]).
:- use_module(planner(ab/objects)).
:- use_module(planner(geometric)).
:- use_module(planner(shot)).
:- use_module(common).
:- use_module(planner(data)).


% Finds a plan for targetting a Pig, returns a list of a target, the strategy chosen, confidence
% plan_(-DecisionList)
plans_common:plan(Bird, plan{bird:Bird, shot:Shot, target_object:Target, impact_angle:ImpactAngle, strategy:"targetPig", confidence:C, reasons:Pigs}) :-
	targetPig(Bird, Target, ImpactAngle, C, Pigs),
	>(C, 0.49),
	shot_params_dict(ImpactAngle, Shot).


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
preferred_shot(_, [P|Pigs], Target, Angle, C) :-
	bounding_box_lst(Pigs, XMin, YMin, XMax, YMax),
	( ((YMax - YMin) > 2*(XMax - XMin)) -> 
		(linksschwein(P,Pigs,Best), 
			flachschuss(Best,BestAngle)),
			(topschwein(P,Pigs,Best), steilschuss(Best,BestAngle))),
	!,
	member(Target, [P|Pigs]),
	(Target == Best -> (Angle is BestAngle, C is 1.0) ; (isHittable(Target, Angle), C is 0.9)).

second_chance(OBJS) :-
	member(O,OBJS),
	(pig(O) ; (findall(O2, isOn(O2,O), ABOVE),second_chance(ABOVE))),
	!.

% direct shot at hittable pigs
targetPig(Bird, Target, Angle, C, [Target]) :-
	hasColor(Bird, Color),
	Color\=white,
	findall(P, pig(P), Pigs),
	all_hittable(Pigs),
	findall(B, bird(B), Birds),
	length(Pigs, Num_Pigs),
	length(Birds, Num_Birds),
	Num_Pigs =< Num_Birds,
	preferred_shot(Bird, Pigs, Target, Angle, C).

% direct shot at hittable pig
targetPig(Bird, Target, Angle, Conf, [Target]) :-
	hasColor(Bird,Color),

	%%%% FIXME: erst zu jedem schwein den flachschuss bestimmen, dann
	%%%%        aus der liste mit paaren per member die schuesse ziehen.
	%%%%        Steilschuesse ggf. als 'last resort' generieren

	Color\=white,
	pig(Target),  %leftmost_pig(Target),
	findall(A,isHittable(Target, A), AS),
	max_list(AS, Angle), 
	(num_of_pigs_remaining(1) -> Conf is 1.0 ; Conf is 0.6).

% penetration shot at pig
targetPig(Bird, Target, Angle, Confidence, [Target]) :-
	pig(Target),
%    \+isHittable(Target,_),
	shot_obstacles(Target, Objects, Angle),
%    writeln(Objects),
	penetration(Bird, Objects, P),
	(second_chance(Objects) -> Bonus is 0.2 ; Bonus is 0.0),
	P =< 1.5,
	((P<1.0) -> Confidence is 1.0+Bonus ; (Confidence is Bonus + 1.0 - 0.33*P)).