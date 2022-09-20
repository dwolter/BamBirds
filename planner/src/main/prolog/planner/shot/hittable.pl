:- module(shot_hittable, [
	all_hittable/1,
	retract_unhittable/0,
	assert_hittable/0,
	assert_parabolas_and_hittable/0
	]).
:- use_module(obstacles).
:- use_module(collision_shapes).
:- use_module(planner(data)).
:- use_module(planner(ab)).
:- use_module(planner(physics/projectile_motion)).
:- use_module(planner(geometric/common)).
:- use_module(planner(geometric/polygon)).
:- use_module(library(uuid)).

all_hittable([]).
all_hittable([T|Ts]) :-
	isHittable(T,_), !,
	all_hittable(Ts).

retract_unhittable :-
	forall(isHittable(Object, UUID),
		(shot_obstacles(Object, [], UUID) ->
			true
			;
			data:retract(isHittable(Object, UUID)))
	).

assert_hittable :-
	forall(shot_obstacles(Object, [], UUID),
		data:assertz(isHittable(Object, UUID))).

assert_parabolas_and_hittable :-
	in_slingshot(Bird),
	forall((
			object(Object), 
			hit_point(Object, [HitX, HitY]), 
			parabola_for_target_point(Bird, [HitX, HitY], ImpactAngle, A, B)
		),
		(
			((
				object_on_parabola(Object, HitX, A, B, ActualHitX),
				parabola_y_value(A,B,ActualHitX,ActualHitY)
			); 
				[ActualHitX,ActualHitY] = [HitX, HitY]
			),
			uuid(UUID),
			
			data:assertz(parabola(Object, UUID, [ActualHitX,ActualHitY], ImpactAngle, A, B)),
			(shot_obstacles(Object, [], UUID)->
				data:assertz(isHittable(Object, UUID))
				;
				true
			)
		)
	).

hit_point(Object, HitPoint) :-
	shape(Object, Shape, X, Y, Area, Spec),
	hit_point(Shape, X, Y, Area, Spec, HitPoint).
hit_point(Shape, X, Y, _, _, [X, Y]) :-
	\+ Shape == poly.
hit_point(ball, X, Y, Area, [R], HitPoint) :-
	( Area > 150 ->
		(
			between(0, 7, K),
			TX is X + ((R) * cos(K * (pi/4))),
			TY is Y + ((R) * sin(K * (pi/4))),
			HitPoint = [TX, TY]
		);
		false
	).
	
hit_point(rect, X, Y, Area, [W, H, A], HitPoint) :-
	Area >= 200,
	XRa is (0.5*H),
	YRa is (0.5*W),
	rot_shift(A, X, Y, [[-XRa,-YRa], [-XRa,YRa], [XRa,YRa], [XRa,-YRa]], Points),
	member(HitPoint, Points).
hit_point(rect, X, Y, Area, [W, H, A], HitPoint) :-
	Area >= 50,
	H > 1.5 * W,
	XRa is (0.25*H),
	rot_shift(A, X, Y, [[-XRa,0], [XRa,0]], Points),
	member(HitPoint, Points).
hit_point(rect, X, Y, Area, [W, H, A], HitPoint) :-
	Area >= 50,
	W > 1.5 * H,
	YRa is (0.3*W),
	rot_shift(A, X, Y, [[0,-YRa], [0,YRa]], Points),
	member(HitPoint, Points).
hit_point(poly, _, _, _Area, [_NPoints | Points], HitPoint) :-
	% TODO: Generate hitpoints on edges in regular distances 
	% between(0, NPoints-1, Index),
	% PointA is nth0(Points, Index),
	% PointB is nth0(Points, (Index+1) mod NPoints),
	% DistanceToNextPoint is distance(PointA, PointB),
	% ( DistanceToNextPoint > 50 ->
	% 	between();
	% 	HitPoint = PointA
	% 	).
	member(HitPoint, Points).

% SHOT==[Pivot_x, Pivot_y, Angle, Hit_X, Hit_Y, [List-of-Obstacles], Tap, Drag_x,Drag_y]
