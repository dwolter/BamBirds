:- module(plans_lr_domino, [domino_last_resort/5, plans_common:plan_last_resort/2, plans_common:plan_last_resort/3]).
:- use_module(planner(shot)).
:- use_module(common).
:- use_module(approximate_shot).
:- use_module(planner(data)).
:- use_module(planner(ab/objects)).


plans_common:plan_last_resort(Bird, plan{bird:Bird, shot:Shot, target_object:Target, impact_angle:ImpactAngle, strategy:"dominoLR", confidence:C, reasons:Reasons}) :-
	domino_last_resort(Bird, Target, UUID, C, Reasons),
	shot_params_dict(UUID, Shot, ImpactAngle).
plans_common:plan_last_resort(Bird, Target, plan{bird:Bird, shot:Shot, target_object:Target, impact_angle:ImpactAngle, strategy:"dominoLR", confidence:C, reasons:Reasons}) :-
	domino_last_resort(Bird, Target, UUID, C, Reasons),
	shot_params_dict(UUID, Shot, ImpactAngle).

% last resort: hit something above left of any pig
domino_last_resort(Bird, Target, UUID, C, [destroy(P)]) :-
	\+ whitebird(Bird),
	pig(P,XP,YP,_,_),
	\+isHittable(P,_),
	hasMaterial(Target,_,XT,YT,_,_),
	% FIXME: at least some threshold should be defined
	XT < XP, % left
	YT < YP, % above (positive Y goes down!)
	(isHittable(Target, PossibleUUID) ->
		UUID=PossibleUUID;
		(
			% FIXME: Why?
			is_goal(Target, _),
			find_shot(Bird, Target, _, UUID)
		)
	),
	(flachschuss(Target, UUID) -> C is 0.51 ; C is 0.5).
