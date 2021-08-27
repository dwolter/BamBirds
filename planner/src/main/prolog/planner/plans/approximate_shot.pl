:- module(plans_approximate_shot, [find_shot/3, plans_common:plan/2]).
:- use_module(common).
:- use_module(planner(shot)).
:- use_module(planner(data)).

%%%
%%% Tweaking shots onto goal
%%%

% Schuss auf nicht so ganz treffbare Dinge einzirkeln
plans_common:plan(Bird, plan{bird:Bird, shot:Shot, target_object:Target, impact_angle:ImpactAngle, strategy:"approximateShot", confidence:C, reasons:Pigs}) :-
	proper_goal(Target, C, Pigs),
	\+ isHittable(Target, _),
	find_shot(Bird, Target, ImpactAngle),
	shot_params_dict(ImpactAngle, Shot).

find_shot(Bird, Goal, ImpactAngle) :-
	hasMaterial(Goal, _, X, Y, W, H),
	shape(Goal, _, CX, CY, _, _),
	XR is (X+W),
	YD is (Y+H),
	member(GX, [X, CX, XR]),
	member(GY, [Y, CY, YD]),
	shots_at_point(Bird, Goal, GX, GY, Shots),
	member([ImpactAngle, Obstacles], Shots),
	( Obstacles=[] ; Obstacles=[[Goal,_, _]|_] ).
