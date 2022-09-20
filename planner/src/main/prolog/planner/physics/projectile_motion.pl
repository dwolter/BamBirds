:- module(physics_projectile_motion, [
	launch_velocity/3,
	predict_launch_params/4,
	angleToParabola/3,
	parabola_for_actual_angle/4,
	angle_to_release_point/4,
	time_of_flight/4,
	parabola_for_target_point/5,
	radiantAtX/4,
	angleAtX/4,
	parabola_y_value/4,
	parabola_is_flat/2
	]).
:- use_module(constants).
:- use_module(planner(geometric/angles)).
:- use_module(planner(geometric/common)).
:- use_module(planner(data)).

%% computes A*X^2 + B*X + C
quadric(X, [A, B, C], Y) :-
	Y is A*X^2 + B*X + C.

quadric_deduction(X, [A, B | _], Y) :-
	Y is 2*A*X + B.

%% computes velocity Vel for bird of color BC launched at Angle
%% ShotHelper angleToVelocity()
launch_velocity(BC, Angle, Vel) :-
	high_angle_begin(BC, AHigh),
	(AHigh =< Angle ->
	high_angle_velocity(BC, PARAMS)
	;
	low_angle_velocity(PARAMS)
	),
	quadric(Angle, PARAMS, V),
	scene_scale(_,S),
	Vel is S*V.

%% computes angles for highh and low shot (ParabolaMath targetToAngles())
target_angles(Vel, TX, TY, A1, A2) :-
	V2 is Vel*Vel,
	ROOTTERM is (V2*V2 - TX*TX - 2*TY*V2),
	ROOTTERM > 0,
	ROOT is (sqrt(ROOTTERM)),
	A1 is atan((V2 - ROOT)/TX),
	A2 is atan((V2 + ROOT)/TX).

%% iterates velocity<->angle estimates I times
%% correct_estimate(bird color, target x, target y, approx. angle, resulting angle, resulting velocity, loop count)
correct_estimate(BC, _, _, A, A, Vel, _, 0) :-
	launch_velocity(BC, A, Vel), !.

correct_estimate(BC, TX, TY, A, CA, Vel, Upper, I) :-
	I>0, !,
%    format('A=~w, CA=~w, Vel=~w, Upper=~w, I=~w~n', [A, CA, Vel, Upper, I]),
	launch_velocity(BC, A, V),
	target_angles(V, TX, TY, CA1, CA2), % angles for upper and lower, select right one
	NI is I-1,
	(Upper ->
 correct_estimate(BC, TX, TY, CA1, CA, Vel, Upper, NI)
	;
	correct_estimate(BC, TX, TY, CA2, CA, Vel, Upper, NI)).

%% computes error as Euclidean difference to goal using high_shot velocity estimates
error_high_estimate(BC, TRX, TRY, ANGLE, ERROR) :-
	high_angle_velocity(BC, PARAMS),
	quadric(ANGLE, PARAMS, V),
	scene_scale(_,S),
	Vel is S*V,
	V1 is cos(ANGLE)*Vel,
	V2 is sin(ANGLE)*Vel,
	A is (-0.5/(V1^2)),
	V1 \= 0,
	B is (V2 / V1),
	ERROR is abs(TRY - A*TRX^2 - B*TRX).

%% binary search for high shot closest to target TRX, TRY
correct_estimate_high_shot(_, _, _, ANGLE, LOWER, UPPER) :-
	abs(UPPER - LOWER) < 1e-4,
	ANGLE is UPPER,
	!.
correct_estimate_high_shot(BC, TRX, TRY, ANGLE, LOWER, UPPER) :-
	abs(UPPER - LOWER) >= 1e-4,
	MID is (0.5*(UPPER+LOWER)),
	MID_PLUS is (MID + 1e-5),
	MID_MINUS is (MID - 1e-5),
	error_high_estimate(BC, TRX, TRY, MID_PLUS , ERR_PLUS ),
	error_high_estimate(BC, TRX, TRY, MID_MINUS, ERR_MINUS),
	(ERR_PLUS > ERR_MINUS ->
 correct_estimate_high_shot(BC, TRX, TRY, ANGLE, LOWER, MID)
	;
	correct_estimate_high_shot(BC, TRX, TRY, ANGLE, MID, UPPER)).

predict_launch_params(BC, TX, TY, [[CA1, V1], [CA2, V2]]) :-
	slingshotPivot(X0, Y0),
	TX > X0,
	scene_scale(S,_),
	TRX is ((TX-X0)/S), % relative target points
	TRY is ((Y0-TY)/S),
%    format('target (relative) = [~w,~w]~n', [TRX, TRY]),
	launch_velocity(BC, 0.6981317007977318, V),   % estimate start params for 40 deg.
	target_angles(V, TRX, TRY, A1, A2),
	high_angle_begin(BC, HA),
	%% compute shot1: angle CA1, velocity V1
	(A1 >= HA ->
 (correct_estimate_high_shot(BC, TRX, TRY, CA1, HA, 1.501),
 launch_velocity(BC, CA1, V1))
	;
	correct_estimate(BC, TRX, TRY, A1, CA1, V1, true, 5)),
	%% compute shot2: angle CA2, velocity V1
	(A2 >= HA ->
 (correct_estimate_high_shot(BC, TRX, TRY, CA2, HA, 1.501),
	launch_velocity(BC, CA2, V2))
	;
	correct_estimate(BC, TRX, TRY, A2, CA2, V2, false, 5)).

radiantAtX(A, B, TX, Radiant) :-
	quadric_deduction(TX, [A, B], Gradient),
	Radiant is atan(Gradient).

angleAtX(A, B, TX, Angle) :-
	radiantAtX(A, B, TX, Radiant),
	radiantToAngle(Radiant, Angle).

parabola_y_value(A, B, TX, TY) :-
	slingshotPivot(X0, Y0),
	X is TX - X0,
	quadric(X, [A, B, 0], Y),
	TY is Y0 - Y.

parabola_for_target_point(Bird, [TX, TY], ImpactAngle, A, B) :-
	hasColor(Bird, BirdColor),
	predict_launch_params(BirdColor, TX, TY, AngleVels),
	member([Angle,_], AngleVels),
	parabola_for_actual_angle(BirdColor, Angle, A, B),
	slingshotPivot(X0, _),
	DXMAX is TX-X0,
	angleAtX(A, B, DXMAX, ImpactAngle).

angleToParabola(Angle, A, B) :-
	angleToRadiant(Angle , Radiant),
	parabola_for_actual_angle(yellow, Radiant ,A ,B ).


%% ParabolaMath velocityComponentsToParabola()
%% NB: call from high_angle error function relying on scale=1 does *not* use this predicate
velocity_components_to_parabola(V1, V2, A, B) :-
	scene_scale(S,_),
	A is (-0.5/(S*V1^2)),
	V1 \= 0,
	B is (V2 / V1).

%% ShotPlanner parabolaForActualAngle()
parabola_for_actual_angle(BC, Angle, A, B) :-
	launch_velocity(BC, Angle, Vel),
	V1 is cos(Angle)*Vel,
	V2 is sin(Angle)*Vel,
	velocity_components_to_parabola(V1, V2, A, B).

angle_to_release_point(BC, Angle, RX, RY) :-
	high_angle_begin(BC, HA), % launch_to_actual()
	(Angle >= HA ->
		(high_angle_change(BC, Params), quadric(Angle, Params, Offset));
		(low_angle_change(Params), quadric(Angle, Params, Offset))
	),
	Corrected is (Angle+Offset),
%    format('Angle ~w --> ~w~n', [Angle, Corrected]),
	slingshotPivot(_,SY),
	(Angle < 0 ->
		% Anything with an end y point < 100 will be discarded by the server
		QuadrantSize is SY - 100;
		QuadrantSize is 1000
	),
	RX is round(-QuadrantSize * cos(Corrected)),
	RY is round(+QuadrantSize * sin(Corrected)).

% computes total time of flight (TOF) based on velocity (V), angle (A),
% distance to travel along X axis (DX)
time_of_flight(V, A, DX, TOF) :-
	scene_scale(S,_),
	TOF is (815 * (DX/S) / (V*cos(A))). % magic 815

parabola_is_flat(A,B) :-
	radiantAtX(A,B,0,Radiant),
	Radiant < (pi / 4).
