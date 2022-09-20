:- module(plans_defrost, [target_ice/4, plans_common:plan/2]).
:- use_module(common).
:- use_module(planner(ab/objects)).
:- use_module(planner(shot)).
:- use_module(planner(data)).

plans_common:plan(Bird,plan{bird:Bird, shot:Shot, target_object:Target, impact_angle:ImpactAngle, strategy:"defrost", confidence:C, reasons:Reasons}) :-
	bluebird(Bird),
	target_ice(Target, UUID, C, Reasons),
	>(C, 0.49),
	shot_params_dict(UUID, Shot, ImpactAngle).

connects(XL, YL, WL, HL, XR, YR, _, HR) :-
	YML is YL + HL div 2,
	YMR is YR + HR div 2,
	abs(YML-YMR) < max(10, 3*(HL+HR)), % roughly same level
	XR - (XL + WL) < 20.

ice_cluster_center(Target, horizontal, C) :-
	hasMaterial(Target, ice, X, Y, W, H),
	once(isHittable(Left, _)),
	hasMaterial(Left, ice, XL, YL, WL, HL),
	connects(XL, YL, WL, HL, X, Y, W, H),
	once(isHittable(Right, _)),
	hasMaterial(Right, ice, XR, YR, WR, HR),
	connects(X, Y, W, H, XR, YR, WR, HR),
	C is 0.7.

%count_blocking_ice(_, [], 0).
%count_blocking_ice(BS, [[_, OBSTACLES] | POBSTS], C) :-
%    count_blocking_ice(BS, POBSTS, C2),
%    intersection(BS, OBSTACLES, BLOCKS),
%    length(BLOCKS, B),
%    C is B+C2.

target_ice(Target, UUID, BestScore, Reasons) :-
	% find trajectories at pigs that need ice to be removed...
	pig(P),
	shot_obstacles(P,Obstacles,UUID),
	parabola(P, UUID, _, PAngle,_,_),
	% ...but without hills inbetween
	\+ (member(H, Obstacles), hill(H)),
	member(Target, Obstacles),
	isHittable(Target, UUID),
	parabola(Target, UUID, _, Angle,_,_),
	Angle < PAngle+10,
	Angle > PAngle-10,
	hasMaterial(Target,ice,_,_,_,_),
	findall(CI, (hasOrientation(Target,HW), ice_cluster_center(Target, HW, CI)), Scores),
	Scores\=[],
	max_list(Scores, BestScore),
	merge_reasons([], [P], [], Reasons).
