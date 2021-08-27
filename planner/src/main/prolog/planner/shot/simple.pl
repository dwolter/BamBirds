:- module(shot_simple, [
	penetration/3,
	max_damage/6,
	shots_at_point/4,
	shots_at_point/5,
	flachschuss/2,
	steilschuss/2,
	shot_params/7,
	shot_params_dict/2
	]).
:- use_module(collision_shapes).
:- use_module(hittable).
:- use_module(planner(common/list)).
:- use_module(planner(physics)).
:- use_module(planner(data)).
:- use_module(planner(ab/relations)).
:- use_module(obstacles).

% Time coefficient for taps for each bird.
% Should be adapted manually for specific use cases!
tap_time(black, 1.05):- !.
tap_time(blue, 0.80):- !.
tap_time(white, 0.90):- !.
tap_time(yellow, 0.85):- !.
tap_time(_, 0.9).

penetration(_, [], 0).
penetration(Bird, [O|OS], E) :-
    penetration(Bird, OS, E2),
    shape(O,_,_,_,AREA,_),
    impact(Bird,O,I),
    REL_IMPACT is (AREA*9.3) / I, %% FIXME: * SCALE^2 REALLY?!
    E is E2 + REL_IMPACT.

deep_shot(Bird, Target, Angle, NewTarget, Damaged, Depth, NewAngle) :-
	hasMaterial(Target, _, TX, _, _, _),
	hasMaterial(NewTarget, _, OX, _, _, _),
	OX > TX,
	shot_obstacles(NewTarget, Damaged, NewAngle),
	abs(NewAngle-Angle) < 0.5236, % 30 deg. deviation
	member(Target, Damaged),
	penetration(Bird, Damaged, E),
	E < 1.0,
	length(Damaged, Depth).

select_max_shot([T,E,D,A], [_,_,D2,_], [T,E,D,A]) :-
	D>D2, !.

select_max_shot([_,_,D,_], [T2,E2,D2,A2], [T2,E2,D2,A2]) :-
	D=<D2, !.

% max_damage(+Bird, +Target, -NewTarget, -Angle, -Extra) tunes a shot at Target to maximum damage,
% while still somehow hitting Target. Extra lists the objects additionally destroyed
%
max_damage(Bird, Target, Angle, NewTarget, NewAngle, Extra) :-
	findall([NT,NTDA, NTD,NTA], deep_shot(Bird, Target, Angle, NT, NTDA, NTD, NTA), DST),
	foldl(select_max_shot, DST, [none,[],0,0], Shot),
	Shot=[NewTarget,Extra,_,NewAngle],
	NewTarget\=none.

%% returns list of shots (typically 2: high and low)
%% shot: [Target_X, Target_Y, release angle, A, B, list of obstacles on parabola
%%        Time of flight, release point x, release point y]
shots_at_point(Bird, TX, TY, SHOTS) :-
	hasColor(Bird, BC),
	predict_launch_params(BC, TX, TY, AngleVels),
	slingshotPivot(X0, _),
	DXMAX is (TX-X0),
	!,
	findall([TX,TY,Angle, A, B,Obstacles,TOF, RX, RY],
		(
			member([Angle,V], AngleVels),
			parabola_for_actual_angle(BC, Angle, A, B),
			shot_obstacles(A, B, DXMAX, Obstacles),
			time_of_flight(V, Angle, DXMAX, TOF),
			angle_to_release_point(BC, Angle, RX, RY)
		),
		SHOTS).

shots_at_point(Bird, Target, TX, TY, SHOTS) :-	
	slingshotPivot(X0, _),
	DXMAX is (TX-X0),
	findall([ImpactAngle, Obstacles],
		(
			parabola_for_target_point(Bird, [TX, TY], ImpactAngle, A, B),
			shot_obstacles(A, B, DXMAX, Obstacles),
			(parabola(Target, [TX, TY], ImpactAngle, _, _) ->
				true;
				data:assertz(parabola(Target, [TX, TY], ImpactAngle, A, B))
			)
		),
		SHOTS).

flachschuss(Target, Angle) :-
	%    findall(A, isHittable(Target, A), As),
	findall(A, shot_obstacles(Target,[], A), As),
	max_list(As, Angle).

steilschuss(Target, Angle) :-
	findall(A, isHittable(Target, A), As),
	min_list(As, Angle).

shot_params(ImpactAngle, SX, SY, DX, DY, TX, TY) :-
	in_slingshot(Bird),
	hasColor(Bird, BC),
	slingshotPivot(SX, SY),
	parabola(_, [TX, TY], ImpactAngle, A, B),
	radiantAtX(A, B, 0, ActualAngle),
	angle_to_release_point(BC, ActualAngle, DX, DY).

shot_params(ImpactAngle, SX, SY, DX, DY, TX, TY, TAP) :-
	in_slingshot(Bird),
	hasColor(Bird, BC),
	slingshotPivot(SX, SY),
	parabola(_, [TX, TY], ImpactAngle, A, B),
	radiantAtX(A, B, 0, ActualAngle),
	angle_to_release_point(BC, ActualAngle, DX, DY),
	launch_velocity(BC, ActualAngle, Velocity),
	time_of_flight(Velocity, ActualAngle, TX - SX, TOF),
	tap_time(BC,TapCoefficient),
	TAP = TapCoefficient * TOF.

shot_params_dict(ImpactAngle, shot{sling_x:X0_INT, sling_y:Y0_INT, drag_x:RX, drag_y:RY, target_x:TX, target_y:TY, tap_time:TAP_INT}) :-
	shot_params(ImpactAngle, SX, SY, DX, DY, TX, TY, TAP),
	X0_INT is round(SX),
	Y0_INT is round(SY),
	RX is round(DX),
	RY is round(DY),
	TAP_INT is round(TAP).
