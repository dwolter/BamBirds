:- module(plans_lr_domino, [domino_last_resort/5, plans_common:plan_last_resort/2, plans_common:plan_last_resort/3]).
:- use_module(planner(shot)).
:- use_module(common).
:- use_module(approximate_shot).
:- use_module(planner(data)).


plans_common:plan_last_resort(Bird, plan{bird:Bird, shot:Shot, target_object:Target, impact_angle:ImpactAngle, strategy:"dominoLR", confidence:C, reasons:Pigs}) :-
	domino_last_resort(Bird, Target, ImpactAngle, C, Pigs),
	shot_params_dict(ImpactAngle, Shot).
plans_common:plan_last_resort(Bird, Target, plan{bird:Bird, shot:Shot, target_object:Target, impact_angle:ImpactAngle, strategy:"dominoLR", confidence:C, reasons:Pigs}) :-
	domino_last_resort(Bird, Target, ImpactAngle, C, Pigs),
	shot_params_dict(ImpactAngle, Shot).

% last resort: hit something above left of any pig
domino_last_resort(Bird, Target, ImpactAngle, C, [P]) :-
	pig(P,XP,YP,_,_),
	\+isHittable(P,_),
	hasMaterial(Target,_,XT,YT,_,_),
	XT < XP, % left
	YT < YP, % above (positive Y goes down!)
	(isHittable(Target, Angle) ->
		ImpactAngle=Angle;
		(
			is_goal(Target, _),
			find_shot(Bird, Target, ImpactAngle)
		)
	),
	(RoundedAngle is round(ImpactAngle), between(-35, 35, RoundedAngle) -> C is 0.51 ; C is 0.5).
