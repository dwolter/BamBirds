:- module(plans_approximate_shot, [find_shot/4, plans_common:plan/2]).
:- use_module(common).
:- use_module(planner(shot)).
:- use_module(planner(ab/objects)).
:- use_module(planner(data)).
:- use_module(planner(geometric/common)).

%%%
%%% Tweaking shots onto goal
%%%

% Schuss auf nicht so ganz treffbare Dinge einzirkeln
plans_common:plan(Bird, plan{bird:Bird, shot:Shot, target_object:Target, impact_angle:ImpactAngle, strategy:"approximateShot", confidence:C, reasons:Reasons}) :-
	\+ whitebird(Bird),
	proper_goal(Target, TargetConfidence, Reasons),
	\+ isHittable(Target, _),
	find_shot(Bird, Target, HitConfidence, UUID),
	shot_params_dict(UUID, Shot, ImpactAngle),
	C is TargetConfidence * HitConfidence.

find_shot(Bird, Goal, C, UUID) :-
	hasMaterial(Goal, _, X, Y, W, H),
	shape(Goal, _, CX, CY, _, _),
	XR is (X+W),
	YD is (Y+H),
	member(GX, [X, CX, XR]),
	member(GY, [Y, CY, YD]),
	shots_at_point(Bird, Goal, GX, GY, Shots),
	member([UUID, Obstacles], Shots),
	( Obstacles=[] ; Obstacles=[[Goal,_, _]|_] ),
	distance([CX,CY],[GX,GY], DistanceToMiddle),
	C is 1 - (DistanceToMiddle / max(W/2, H/2)) * 0.1.
